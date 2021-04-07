-- 1
-- used array for courseareas
-- array cant be null for instructor and manager
-- array must be null for administrators
-- format of category must be correct
CREATE OR REPLACE FUNCTION add_employee(name TEXT, address TEXT, phone INTEGER, email TEXT, salary NUMERIC, join_date Date, category TEXT, courseareas TEXT ARRAY)
RETURNS VOID AS $$
DECLARE
    id INT;
    length INT;
    counter INT;
BEGIN
    length := array_length(courseareas, 1);
    counter := 1;
    IF (category = 'Manager') THEN
        IF (length IS NULL) THEN
            RAISE EXCEPTION 'Course areas cannot be empty for managers.';
        END IF;
        INSERT INTO Employees (name, phone, email, join_date, address, depart_date) values (name, phone, email, join_date, address, NULL);
        id := (SELECT eid FROM Employees ORDER BY eid DESC LIMIT 1);
        INSERT INTO Full_time_Emp (eid, monthly_rate) values (id, salary);
        INSERT INTO Managers (eid) values (id);
            IF counter < length THEN
                LOOP
                    EXIT WHEN counter > length;
                    INSERT INTO Course_areas (name, eid) values (courseareas[counter], id);
                    counter := counter + 1;
                END LOOP;
            END IF;
    ELSEIF (category = 'Administrator')  THEN
        IF (length > 0) THEN
            RAISE EXCEPTION 'Course areas must be empy for administrator.';
        END IF;
        INSERT INTO Employees (name, phone, email, join_date, address, depart_date) values (name, phone, email, join_date, address, NULL);
        id := (SELECT eid FROM Employees ORDER BY eid DESC LIMIT 1);
        INSERT INTO Full_time_Emp (eid, monthly_rate) values (id, salary);
        INSERT INTO Administrators (eid) values (id);
    ELSEIF (category = 'Instructor') THEN
        IF (length IS NULL) THEN
            RAISE EXCEPTION 'Course areas cannot be empty for instructors.';
        END IF;
        INSERT INTO Employees (name, phone, email, join_date, address, depart_date) values (name, phone, email, join_date, address, NULL);
        id := (SELECT eid FROM Employees ORDER BY eid DESC LIMIT 1);
        IF counter < length THEN
            LOOP
                EXIT WHEN counter > length;
                INSERT INTO Instructors (eid, name) values (id, courseareas[counter]);
                counter := counter + 1;
            END LOOP;
        END IF;
        IF (salary <= 20) THEN -- part time
            INSERT INTO Part_time_Emp (eid, hourly_rate) values (id, salary);
            INSERT INTO Part_time_instructors (eid) values (id);
        ELSE -- full time
            INSERT INTO Full_time_Emp (eid, monthly_rate) values (id, salary);
            INSERT INTO Full_time_instructors (eid) values (id);
        END IF;
    ELSE
        RAISE EXCEPTION 'Format of category can only be Manager, Administrator or Instructor.';
    END IF;
END;
$$ LANGUAGE plpgsql;

-- 2
CREATE OR REPLACE FUNCTION remove_employee(input_eid INTEGER, input_depart_date Date) RETURNS VOID AS $$
BEGIN
    IF ((select depart_date from Employees where eid = input_eid) IS NOT NULL) THEN
        RAISE EXCEPTION 'This employee has already been removed.';
    ELSEIF ((select count(*) from Managers where eid = input_eid) = 1) THEN -- is a manager
        IF ((select count(*) from Course_areas where eid = input_eid) = 0) THEN -- not managing any area
            UPDATE Employees 
            SET depart_date = input_depart_date
            WHERE eid = input_eid;
        END IF;
        RAISE EXCEPTION 'This employee is a manager that is currently managing an area and cannot be removed.';
    ELSEIF ((select count(*) from Administrators where eid = input_eid) = 1) THEN -- is an administrator
        IF ((select count(*) from (select * from Offerings where eid = input_eid and registration_deadline < input_depart_date) as temp) = 0) THEN -- not managing any area
            UPDATE Employees 
            SET depart_date = input_depart_date
            WHERE eid = input_eid;
        END IF;
        RAISE EXCEPTION 'This employee is an administrator that is currently handling a course offering and cannot be removed.';
    ELSEIF ((select count(*) from Instructors where eid = input_eid) = 1) THEN -- is an instructor
        IF ((select count(*) from (select date from Sessions where eid = input_eid and date > input_depart_date) as temp) = 0) THEN
            UPDATE Employees 
            SET depart_date = input_depart_date
            WHERE eid = input_eid;
        END IF;
        RAISE EXCEPTION 'This employee is an instructor that is currently teaching a course session and cannot be removed.';
    END IF;
END;
$$ LANGUAGE plpgsql;

-- 3
CREATE OR REPLACE FUNCTION add_customer(input_name TEXT, input_address TEXT, input_phone INTEGER, 
input_email TEXT, input_number TEXT, input_CVV INTEGER, input_expiry_date Date) RETURNS VOID AS $$
DECLARE
    cid INTEGER;
BEGIN
    INSERT INTO Customers (name, email, address, phone) values (input_name, input_email, input_address, input_phone);
    cid := (SELECT cust_id FROM Customers ORDER BY cust_id DESC LIMIT 1);
    INSERT INTO Credit_cards (number, CVV, expiry_date, cust_id) values (input_number, input_CVV, input_expiry_date, cid);
    INSERT INTO Owns (number, cust_id, from_date) values (input_number, cid, CURRENT_DATE);
END;
$$ LANGUAGE plpgsql;

-- 4 need to retest after update on cascade command
CREATE OR REPLACE PROCEDURE update_credit_card(input_cust_id INTEGER, input_number TEXT, input_CVV INTEGER, input_expiry_date Date) AS $$
    UPDATE Credit_cards
    SET number = input_number, CVV = input_CVV, expiry_date = input_expiry_date
    WHERE cust_id = input_cust_id;
$$ LANGUAGE SQL;

-- 5
CREATE OR REPLACE PROCEDURE add_course(title TEXT, description TEXT, area TEXT, duration NUMERIC) AS $$
    INSERT INTO Courses (title, duration, description, name) values (title, duration, description, area);
$$ LANGUAGE SQL;

-- 6
CREATE OR REPLACE FUNCTION find_instructors(input_course_id INTEGER, session_date Date, input_start_time INTEGER) 
RETURNS TABLE(output_eid INTEGER, output_name TEXT) AS $$
BEGIN
    -- instructors who can teach
  RETURN QUERY
    (select eid, name 
    from employees 
    where eid in (
        select eid
        from instructors
        where name in (
            select name 
            from courses 
            where courses.course_id = input_course_id
            ))
    except
    -- instructors who cannot teach
    SELECT eid, name
    FROM Employees
    WHERE eid in (
        SELECT eid
        FROM Sessions
        WHERE (Sessions.date = session_date 
        and (input_start_time + (SELECT duration FROM Courses WHERE courses.course_id = input_course_id)) > Sessions.start_time 
        and input_start_time < Sessions.end_time)));
END;
$$ LANGUAGE plpgsql;

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















-- 26
-- returns exactly 6 months, qns wants 6 months without caring about exact?
CREATE OR REPLACE FUNCTION promote_courses() 
RETURNS TABLE(output_cust_id INTEGER, output_cust_name TEXT, output_course_area TEXT, output_course_id INTEGER, output_title TEXT, output_launch_date Date,
output_registration_deadline Date, output_fees NUMERIC) AS $$
BEGIN
    RETURN QUERY
    select * from (select temp.cust_id, c.name, temp.name, courses.course_id, courses.title, o.launch_date, o.registration_deadline, o.fees
    from (select temp.cust_id, c.name from (SELECT * 
    FROM (SELECT ROW_NUMBER() OVER (PARTITION BY cust_id order by date DESC) as r, t.*
    FROM (select * from (select cust_id from (select *, 
    case when 1=1 then CURRENT_DATE - interval '6 months' end as inactive from registers) as temp 
    where launch_date < inactive) as temp natural left outer join registers r) as t ) x
    WHERE x.r <= 3) as temp natural join courses c
    union                                         
    select not_registered.cust_id, ca.name from (select distinct cust_id from customers except select cust_id from registers) as not_registered 
    cross join course_areas ca) as temp inner join customers c on temp.cust_id = c.cust_id inner join courses on temp.name = courses.name natural join offerings o) as temp
    where CURRENT_DATE < registration_deadline
    order by cust_id, registration_deadline;
END;
$$ LANGUAGE plpgsql;

-- 28 i cant test this LOL i need more data got a feeling its not very correct
CREATE OR REPLACE FUNCTION popular_courses() RETURNS 
TABLE (output_course_id INTEGER, output_title TEXT, output_area TEXT, output_num_offerings INTEGER, output_registration INTEGER) AS $$
DECLARE
    curs CURSOR FOR (select course_id, count, title, name
    from (select * from
    (select course_id, start_date, count from
    (select r.course_id, r.launch_date, count(*) 
    from registers r 
    where (select extract(year from date) = (select extract(year from current_date))) 
    group by r.course_id, r.launch_date) as temp natural join offerings o) as temp
    where course_id in
    (select course_id from (select course_id, count(*) from offerings 
    where (select extract(year from start_date) = (select extract(year from current_date))) 
    group by course_id) as temp where count > 1) order by course_id, start_date) as temp natural join
    courses c);
    r RECORD;
    prv_total INTEGER;
    prv_courseid INTEGER;
    prv_title TEXT;
    prv_area TEXT;
    add_record INTEGER; -- 0 dont add, 1 add
    num_offerings INTEGER;
BEGIN
    prv_total := 0;
    prv_courseid := 0;
    prv_title := NULL;
    prv_area := NULL;
    add_record := 0;
    num_offerings := 0;
    OPEN curs;
    LOOP
        FETCH curs INTO r;
        EXIT WHEN NOT FOUND;
        IF prv_courseid != r.course_id and add_record = 1 THEN
            output_course_id := prv_courseid;
            output_title := prv_title;
            output_area := prv_area; 
            output_num_offerings := num_offerings;
            output_registration := prv_total;
            num_offerings := 0;
            add_record := 0;
            RETURN NEXT;
        ELSEIF prv_courseid != r.course_id THEN
            num_offerings := 0;
            add_record := 0;
        ELSEIF prv_courseid = r.course_id THEN
            IF r.count > prv_total THEN
                add_record := 1;
            ELSEIF r.count <= prv_total THEN
                add_record := 0;
            END IF;
        END IF;
        prv_courseid := r.course_id;
        prv_total := r.count;
        prv_title := r.title;
        prv_area := r.name;
        num_offerings := num_offerings + 1;
    END LOOP;
    CLOSE curs;
END;
$$ LANGUAGE plpgsql;

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