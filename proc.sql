
-- 8
-- If session_duration ends after close/rest time return empty
-- filter date
	-- iterate through room
		-- iterate through session, find sessions that clash
		-- if no session clash, add this room to list to return
CREATE OR REPLACE FUNCTION find_rooms 
(input_date DATE, input_start_time INTEGER, input_duration INTEGER)
RETURNS TABLE(rid INTEGER) AS $$
BEGIN
	IF (input_start_time < 0 or input_start_time > 23) THEN
		RAISE EXCEPTION 'Invalid start_time';
		RETURN;
	END IF;
	IF (input_duration < 0) THEN
		RAISE EXCEPTION 'Negative duration';
		RETURN;
	END IF;
	
	RETURN QUERY
		SELECT * 
		FROM (SELECT distinct R.rid
			FROM Rooms R 
			except
			SELECT S.rid
			FROM Sessions S
			WHERE (S.date = input_date and
				(input_start_time + input_duration) > S.start_time and
				input_start_time < S.end_time)) as rooms 
		ORDER BY rooms.rid ASC;
END;
$$ LANGUAGE PLPGSQL;

-- 9
-- iterate through DATE
	-- iterate through rooms
		-- INT[]
		-- iterate through 0-23
		-- If room is available on this hour, append to int[]
		-- Add tuple to list to return
		-- select * from get_available_rooms(date'2021-03-02', date'2021-03-05');
CREATE OR REPLACE FUNCTION get_available_rooms 
(start_date DATE, end_date DATE)
RETURNS TABLE(output_rid INTEGER, output_room_capacity INTEGER, 
day DATE, hours INTEGER[]) AS $$
DECLARE
	iter_hour INTEGER;
	iter_date DATE;
	curr_hours INTEGER[];
	curs CURSOR FOR (SELECT rid, seating_capacity FROM Rooms);
	r RECORD;
BEGIN
	IF (start_date > end_date) THEN
		RAISE EXCEPTION 'End date before start';
		RETURN;
	END IF;
	iter_hour := 9;
	iter_date := start_date;
	curr_hours := '{}';
	OPEN curs;
	LOOP
		FETCH curs INTO r;
		EXIT WHEN NOT FOUND;	
		
		LOOP
			EXIT WHEN iter_date > end_date;
			day := iter_date;		

			output_rid := r.rid;
			output_room_capacity := r.seating_capacity;
			
			LOOP
				EXIT WHEN iter_hour > 17;
				
				IF (iter_hour = 12) THEN
					iter_hour := 14;
				END IF;

				IF (SELECT EXISTS (SELECT r.rid INTERSECT (SELECT find_rooms(iter_date, iter_hour, 1)))) THEN
					curr_hours := array_append(curr_hours, iter_hour);
					
				END IF;
				
			iter_hour := iter_hour + 1;
			
			END LOOP;
			
			hours := curr_hours;
			RETURN NEXT;
			curr_hours := '{}';
			iter_hour := 9;
			iter_date := (SELECT iter_date + integer '1');
			
		END LOOP;
		iter_date := start_date;
	END LOOP;
	CLOSE curs;
END;
$$ LANGUAGE PLPGSQL;

-- 10
-- call add_course_offering(2,100,date'2021-04-06', date'2021-04-06', 90, '{{2021-04-16, 9, 2}}');
CREATE OR REPLACE PROCEDURE add_course_offering 
(c_id INTEGER, course_fees NUMERIC, launch DATE, 
reg_deadline DATE, admin_eid INTEGER, sessions varchar[][]) AS $$

DECLARE 
	hours INTEGER[];
	m   varchar[];
	start_date_iter DATE;
	end_date_iter DATE;
	seating_capacity_iter INTEGER;
	course_duration INTEGER;
	curs_FI CURSOR FOR (SELECT eid FROM instructors ORDER BY eid asc);
	
	rrr INTEGER;
	date_slice DATE;
	hour_slice INTEGER;
	rid_slice INTEGER;
	chosen INTEGER;
BEGIN

	IF (course_fees < 0) THEN
		RAISE EXCEPTION 'Fees cannot be negative';
	END IF;
	
	IF (SELECT NOT EXISTS (SELECT 1 FROM Administrators WHERE eid = admin_eid)) THEN
		RAISE EXCEPTION 'Employee is not an administrator';
	END IF;
	
	IF (COALESCE(array_length(sessions, 1), -1) < 1) THEN
		RAISE EXCEPTION 'Empty session';
	END IF;
	
	course_duration := (SELECT duration FROM Courses WHERE course_id = c_id);
	start_date_iter := sessions[1][1];
	end_date_iter := sessions[1][1];
	seating_capacity_iter := 0;

	FOREACH m SLICE 1 IN ARRAY sessions -- date, start_hour, rid
	LOOP
		date_slice := m[1]::DATE;
		hour_slice := m[2]::INTEGER;
		rid_slice := m[3]::INTEGER;
		
		IF (SELECT NOT EXISTS (SELECT rid_slice INTERSECT (SELECT find_rooms(date_slice, hour_slice, course_duration)))) THEN
			RAISE EXCEPTION 'Room unavailable for this session';
		END IF;
		
		seating_capacity_iter := seating_capacity_iter + (SELECT seating_capacity FROM Rooms R WHERE R.rid = rid_slice);
		
		IF (date_slice < start_date_iter) THEN
			start_date_iter := date_slice;
		END IF;
		
		IF (date_slice > end_date_iter) THEN
			end_date_iter := date_slice;
		END IF;
		
	END LOOP;
	
	IF (reg_deadline <> start_date_iter - 10) THEN
		RAISE EXCEPTION 'Incorrect start date or registration deadline';
	END IF;
	
	insert into Offerings (launch_date, start_date, end_date, registration_deadline, target_number_registrations, seating_capacity, fees, eid, course_id) 
		values (launch, start_date_iter, end_date_iter, reg_deadline, seating_capacity_iter, seating_capacity_iter, course_fees, admin_eid, c_id);

	FOREACH m SLICE 1 IN ARRAY sessions -- date, start_hour, rid
	LOOP
		date_slice := m[1]::DATE;
		hour_slice := m[2]::INTEGER;
		rid_slice := m[3]::INTEGER;  -- Choose instructor with lowest eid
		
		OPEN curs_FI;
		chosen = -1;
		LOOP
		
			FETCH curs_FI INTO rrr;
			EXIT WHEN NOT FOUND;
			EXIT WHEN chosen <> -1;

			IF (rrr in (select eid from instructors where name in (select name from courses where courses.course_id = c_id))) THEN
				IF (rrr not in (SELECT eid FROM Sessions WHERE (Sessions.date = date_slice and (hour_slice + (SELECT duration FROM Courses WHERE Courses.course_id = c_id)) > Sessions.start_time and hour_slice < Sessions.end_time))) THEN
					chosen := rrr;
				END IF;
				RETURN;
			END IF;
		END LOOP;
		CLOSE curs_FI;
		
		IF (chosen = -1) THEN
			RAISE EXCEPTION 'No suitable instructor assignment';
		END IF;
		
		insert into Sessions (date, end_time, start_time, launch_date, course_id, rid, eid) 
			values (date_slice, (hour_slice + course_duration), hour_slice, launch, c_id, rid_slice, chosen);
		-- I am NOT going to enumerate through all possible instructor sorry a bit fked up
		
	END LOOP;
END;
$$ LANGUAGE PLPGSQL;

-- 11
CREATE OR REPLACE PROCEDURE add_course_package
(package_name TEXT, num_free INTEGER, sale_start DATE, sale_end DATE, fee NUMERIC) AS $$

BEGIN
	IF (sale_start > sale_end) THEN
		RAISE EXCEPTION 'End date before start';
	END IF;
	
	IF (fee < 0) THEN
		RAISE EXCEPTION 'Negative price';
	END IF;
	
	IF (num_free <= 0) THEN
		RAISE EXCEPTION 'There must be at least 1 free session';
	END IF;
	
	IF (CURRENT_DATE > sale_start) THEN
		RAISE EXCEPTION 'Please put a sale start date after today';
	END IF;
	
	IF (CURRENT_DATE > sale_end) THEN
		RAISE EXCEPTION 'Please put a sale end date after today';
	END IF;
	
	insert into Course_packages (sale_start_date, num_free_registrations, name, sale_end_date, price) 
		values (sale_start, num_free, package_name, sale_end, fee);
END;
$$ LANGUAGE PLPGSQL;

-- 12
CREATE OR REPLACE FUNCTION get_available_course_packages ()
RETURNS TABLE(package_name TEXT, num_free_sessions INTEGER,
END_DATE DATE, fee NUMERIC) AS $$
	SELECT cp.name, cp.num_free_registrations, cp.sale_end_date, cp.price
	FROM course_packages cp
	WHERE (sale_start_date <= CURRENT_DATE)
		and (sale_end_date >= CURRENT_DATE);
$$ LANGUAGE SQL;

-- 13
CREATE OR REPLACE PROCEDURE buy_course_package
(input_cust_id INTEGER, input_package_id INTEGER) AS $$
BEGIN

	IF (SELECT NOT EXISTS (SELECT 1 FROM Customers WHERE cust_id = input_cust_id)) THEN
		RAISE EXCEPTION 'Customer does not exist';
	END IF;
	
	IF (SELECT NOT EXISTS (SELECT 1 FROM Owns WHERE cust_id = input_cust_id)) THEN
		RAISE EXCEPTION 'Customer does not own a credit card';
	END IF;
	
	IF (SELECT EXISTS (SELECT 1 FROM Buys WHERE (cust_id = input_cust_id) and (num_remaining_redemptions > 0))) THEN
		RAISE EXCEPTION 'Customer has an active package';
	END IF;
	
	-- Here, num_remaining_redemption must be equal to 0
	IF (SELECT EXISTS (SELECT 1 FROM (Redeems natural join Sessions) as RS WHERE (RS.cust_id = input_cust_id) and (RS.date >= current_date + 7))) THEN
		RAISE EXCEPTION 'Customer has a partially active package';
	END IF;
	
	insert into Buys (buy_date, num_remaining_redemptions, cust_id, number, package_id) 
		values (CURRENT_DATE, 
			(select num_free_registrations from Course_packages where package_id = input_package_id), 
			input_cust_id, 
			(SELECT number FROM Owns WHERE input_cust_id = cust_id ORDER BY from_date desc limit 1), 
			input_package_id);

END;
$$ LANGUAGE PLPGSQL;

















--30
CREATE OR REPLACE FUNCTION view_manager_report() 
RETURNS TABLE(manager_name TEXT, course_area_managed INTEGER,
course_offerings_ended_same_year INTEGER, total_fees NUMERIC,
course_offering_highest_total_fees TEXT[]) AS $$

DECLARE
	report_curs CURSOR FOR (SELECT eid FROM Managers natural join Employees ORDER BY name asc);
	manager_id INTEGER;
	fee NUMERIC;
BEGIN

	OPEN report_curs;
	LOOP
		FETCH report_curs INTO manager_id;
		EXIT WHEN NOT FOUND;
		
		manager_name := (SELECT name FROM Employees WHERE Employees.eid = manager_id);

		course_area_managed := (SELECT count(*) FROM Course_areas WHERE Course_areas.eid = manager_id);
		
		course_offerings_ended_same_year := (SELECT count(*) FROM ((Offerings natural join Courses) OC inner join Course_areas C on OC.name = C.name) WHERE C.eid = manager_id and date_part('year', current_date) = date_part('year', OC.end_date));
		
		-- Package fees excluding cancelled with package credit
		total_fees := COALESCE((
			WITH NOREFUNDS as (
				SELECT distinct cust_id, sid, launch_date, course_id
				FROM Redeems R
				except
				SELECT cust_id, sid, launch_date, course_id
				FROM Cancels C
				WHERE COALESCE(C.package_credit, 0) = 1
			)
			SELECT SUM(FLOOR(RCO.price/RCO.num_free_registrations))
			FROM (NOREFUNDS natural join Redeems natural join Course_packages natural join Offerings) RCO, Courses CO, Course_areas C
			WHERE C.eid = manager_id and date_part('year', current_date) = date_part('year', RCO.end_date) and RCO.course_id = CO.course_id and CO.name = C.name
		), 0);
		
		-- Registration fees
		total_fees := total_fees + COALESCE((
			WITH Registrants as (
				SELECT distinct cust_id, number, sid, launch_date, course_id
				FROM Registers
				except
				SELECT cust_id, number, sid, launch_date, course_id
				FROM Redeems
			)
			SELECT SUM(OC.fees)
			FROM ((Registrants natural join Registers natural join Offerings natural join Courses) OC inner join Course_areas C on OC.name = C.name)
			WHERE C.eid = manager_id and date_part('year', current_date) = date_part('year', OC.end_date) 
		), 0);
		
		-- Registration fees for cancelled 
		total_fees := total_fees - COALESCE((
			SELECT sum(OC.refund_amt)
			FROM ((Cancels natural join Offerings natural join Courses) OC inner join Course_areas C on OC.name = C.name)
			WHERE COALESCE(OC.refund_amt, -1) <> -1 AND C.eid = manager_id and date_part('year', current_date) = date_part('year', OC.end_date)
		), 0);
		
		course_offering_highest_total_fees := ARRAY(
			WITH package_fee as (
				-- Same as above, but instead of getting the sum, I return multiple tuples of title + fees
				WITH NOREFUNDS as (
					SELECT distinct cust_id, sid, launch_date, course_id
					FROM Redeems R
					except
					SELECT cust_id, sid, launch_date, course_id
					FROM Cancels C
					WHERE COALESCE(C.package_credit, 0) = 1
				)
				SELECT (CO.title) as title, (FLOOR(RCO.price/RCO.num_free_registrations)) as fees
				FROM (NOREFUNDS natural join Redeems natural join Course_packages natural join Offerings) RCO, Courses CO, Course_areas C
				WHERE C.eid = manager_id and date_part('year', current_date) = date_part('year', RCO.end_date) and RCO.course_id = CO.course_id and CO.name = C.name
			), registration_fee as (
				-- Same as above, but instead of getting the sum, I return multiple tuples of title + fees
				WITH Registrants as (
					SELECT distinct cust_id, number, sid, launch_date, course_id
					FROM Registers
					except
					SELECT cust_id, number, sid, launch_date, course_id
					FROM Redeems
				)
				SELECT (OC.title) as title, (OC.fees) as fees
				FROM ((Registrants natural join Registers natural join Offerings natural join Courses) OC inner join Course_areas C on OC.name = C.name)
				WHERE C.eid = manager_id and date_part('year', current_date) = date_part('year', OC.end_date) 
			), register_cancel as (
				-- Same as above, but instead of getting the sum, I return multiple tuples of title + fees
				SELECT (OC.title) as title, (OC.refund_amt * -1) as fees
				FROM ((Cancels natural join Offerings natural join Courses) OC inner join Course_areas C on OC.name = C.name)
				WHERE COALESCE(OC.refund_amt, -1) <> -1 AND C.eid = manager_id and date_part('year', current_date) = date_part('year', OC.end_date)
			)
			SELECT title
			FROM (SELECT * from package_fee 
				union SELECT * from registration_fee union SELECT * FROM register_cancel) as tgt
			GROUP BY title
			HAVING sum(fees) >=
				(SELECT max(summ)
				FROM (
					SELECT sum(tgt2.fees) as summ
					FROM (SELECT * from package_fee 
					union SELECT * from registration_fee union SELECT * FROM register_cancel) as tgt2
					GROUP BY tgt2.title) as innerGB
				)
		);
		
		raise notice '%', total_fees;
		
		RETURN NEXT;
		
	END LOOP;
	CLOSE report_curs;
		
END;
$$ LANGUAGE PLPGSQL;