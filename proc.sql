-- Function 1
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

-- Function 2
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

-- Function 3
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

-- Function 4
-- need to retest after update on cascade command
CREATE OR REPLACE PROCEDURE update_credit_card(input_cust_id INTEGER, input_number TEXT, input_CVV INTEGER, input_expiry_date Date) AS $$
    UPDATE Credit_cards
    SET number = input_number, CVV = input_CVV, expiry_date = input_expiry_date
    WHERE cust_id = input_cust_id;
$$ LANGUAGE SQL;

-- Function 5
CREATE OR REPLACE PROCEDURE add_course(title TEXT, description TEXT, area TEXT, duration NUMERIC) AS $$
    INSERT INTO Courses (title, duration, description, name) values (title, duration, description, area);
$$ LANGUAGE SQL;

-- Function 6
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
            )) AND session_date <= employees.depart_date
    except
    -- instructors who cannot teach
    SELECT eid, name
    FROM Employees
    WHERE eid in (
        SELECT eid
        FROM Sessions
        WHERE (Sessions.date = session_date 
      	and (input_start_time + (SELECT duration FROM Courses WHERE Courses.course_id = input_course_id)) > Sessions.start_time 
      	and input_start_time < Sessions.end_time)));
END;
$$ LANGUAGE plpgsql;

-- Function 8
-- Example call: 
-- select * from find_rooms(date'2021-04-13', 9, 3);
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

-- Function 9
-- Example call: 
-- select * from get_available_rooms(date'2021-04-22', date'2021-04-28');
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

-- Function 10
-- Example call: 
-- call add_course_offering(3, 300, date'2021-04-13', date'2021-04-16', 85, '{{2021-04-26, 9, 2}, {2021-04-27, 9, 3}, {2021-04-28, 9, 4}}');
-- Session is a 2D array where each nested array contains {<Session date>, <Session start hour>, <Room identifier>}
CREATE OR REPLACE PROCEDURE add_course_offering 
(c_id INTEGER, course_fees NUMERIC, launch DATE, 
reg_deadline DATE, target_reg INTEGER, admin_eid INTEGER, sessions varchar[][]) AS $$

DECLARE 
	hours INTEGER[];
	m varchar[];
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
	
  insert into Offerings (launch_date, start_date, end_date, registration_deadline, target_number_registrations, seating_capacity, fees, eid, course_id) 
    values (launch, null, null, reg_deadline, target_reg, 0, course_fees, admin_eid, c_id);

	FOREACH m SLICE 1 IN ARRAY sessions -- date, start_hour, rid
	LOOP
		date_slice := m[1]::DATE;
		hour_slice := m[2]::INTEGER;
		rid_slice := m[3]::INTEGER;  -- Choose instructor with lowest eid

    IF (SELECT NOT EXISTS (SELECT rid_slice INTERSECT (SELECT find_rooms(date_slice, hour_slice, course_duration)))) THEN
			RAISE EXCEPTION 'Room unavailable for this session';
		END IF;
		
		OPEN curs_FI;
		chosen = -1;
		LOOP
		
			FETCH curs_FI INTO rrr;
			EXIT WHEN NOT FOUND;
			EXIT WHEN chosen <> -1;

			IF (rrr in (select eid from instructors where name in (select name from courses where courses.course_id = c_id))) THEN
				IF (rrr not in (SELECT eid FROM Sessions WHERE (Sessions.date = date_slice and (hour_slice + (SELECT duration FROM Courses WHERE Courses.course_id = c_id)) > Sessions.start_time and hour_slice < Sessions.end_time))) THEN
					IF (rrr in (select eid from Employees e where date_slice <= e.depart_date)) THEN
            chosen := rrr;
          END IF;
          RETURN;
				END IF;
			END IF;
		END LOOP;
		CLOSE curs_FI;
		
		IF (chosen = -1) THEN
			RAISE EXCEPTION 'No suitable instructor assignment';
		END IF;
		
		insert into Sessions (date, end_time, start_time, launch_date, course_id, rid, eid) 
			values (date_slice, (hour_slice + course_duration), hour_slice, launch, c_id, rid_slice, chosen);
		
	END LOOP;
END;
$$ LANGUAGE PLPGSQL;

-- Function 11
-- Example call: 
-- call add_course_package('Almight package', 5, date'2021-05-01', date'2021-05-29', 500);
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
		RAISE NOTICE 'Be careful! Sale start date after today';
	END IF;
	
	IF (CURRENT_DATE > sale_end) THEN
		RAISE NOTICE 'Be careful! Sale end date after today';
	END IF;
	
	insert into Course_packages (sale_start_date, num_free_registrations, name, sale_end_date, price) 
		values (sale_start, num_free, package_name, sale_end, fee);
END;
$$ LANGUAGE PLPGSQL;

-- Function 12
-- Example call: 
-- select * from get_available_course_packages();
CREATE OR REPLACE FUNCTION get_available_course_packages ()
RETURNS TABLE(package_name TEXT, num_free_sessions INTEGER,
END_DATE DATE, fee NUMERIC) AS $$
	SELECT cp.name, cp.num_free_registrations, cp.sale_end_date, cp.price
	FROM course_packages cp
	WHERE (sale_start_date <= CURRENT_DATE)
		and (sale_end_date >= CURRENT_DATE);
$$ LANGUAGE SQL;

-- Function 13
-- Example call: 
-- call buy_course_package(9, 1);
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

-- Function 14
-- Get course packages
create or replace function get_my_course_package(cust_id integer)
returns json
as $$
declare
    input_id integer := cust_id;
begin
    return (
        with cte as (
        select *
        from Redeems R join Sessions S on (R.sid = S.sid and R.launch_date = S.launch_date and R.course_id = S.course_id)
        where R.cust_id = input_id
        order by S.date, S.start_time asc
    )
   
    select json_build_object('package_name', package_name, 'purchase_date', purchase_date, 
    'price', price, 'num_free_sessions', num_free_sessions, 
    'num_redemptions_left', num_free_sessions - (select count(*) from cte), 
    'redeemed_sessions', (select json_agg(cte) from cte))
    from (
        select 
            P.name as package_name, 
            B.buy_date as purchase_date,
            P.price as price,
            P.num_free_registrations as num_free_sessions
        from Course_packages P join Buys B on (P.package_id = B.package_id) 
        join Redeems R on (B.cust_id = R.cust_id) 
        where B.cust_id = input_id
    ) as X
    );
end;
$$ Language plpgsql; 

-- Function 15
-- Get available course offerings
create or replace function get_available_course_offerings()
returns TABLE(course_title TEXT, course_area TEXT, start_date DATE, 
end_date DATE, reg_deadline DATE, course_fees NUMERIC, remaining_seats INT) 
AS $$
declare
    curs cursor for (
        select 
            title as course_title,
            name as course_area,
            O.start_date as begin_date,
            O.end_date as final_date,
            O.registration_deadline as reg_deadline,
            O.fees as course_fees,
            O.seating_capacity,
            C.course_id,
            O.launch_date
        from 
            Offerings O join Courses C on (O.course_id = C.course_id)
        where current_date + 10 < reg_deadline
        order by reg_deadline, title asc
    );
    r record;
begin
    open curs;
    loop 
        fetch curs into r;
        exit when not found;
        course_title := r.course_title;
        course_area := r.course_area;
        start_date := r.begin_date;
        end_date := r.final_date;
        reg_deadline := r.reg_deadline;
        course_fees := r.course_fees;
        remaining_seats := r.seating_capacity - (
            select count(*)
            from 
                Sessions S join Rooms Ro on (S.rid = Ro.rid)
                join Offerings O on (O.course_id = S.course_id and O.launch_date = S.launch_date)
            group by O.course_id, O.launch_date
            having O.course_id = r.course_id and O.launch_date = r.launch_date
        );
        return next;
    end loop;
    close curs;
end;
$$ Language plpgsql;

-- Function 16
-- Get available course sessions
create or replace function get_available_course_sessions(input_id in integer, input_date in date)
returns TABLE(session_date DATE, start_hour integer, instr_name TEXT, remaining_seats integer)
AS $$
declare
    curs cursor for (
        select
            start_time,
            I.name as instr_name,
            S.date as session_date,
            R.seating_capacity as session_capacity,
            S.sid as session_id,
            S.launch_date as offering_launch_date,
            S.course_id as session_course_id
        from Sessions S join Offerings O on (S.course_id = O.course_id and S.launch_date = O.launch_date)
        join Rooms R on (R.rid = S.rid) join Instructors I on (S.eid = I.eid)
        where O.course_id = input_id and O.launch_date = input_date
        order by session_date, start_time asc
    );
    r record;
begin   
    open curs;
    loop
        fetch curs into r;
        exit when not found;
        session_date := r.session_date;
        start_hour := r.start_time;
        instr_name := r.instr_name;
        remaining_seats := r.session_capacity - (
            select count(*)
            from Registers Re
            where Re.sid = r.session_id and Re.launch_date = r.offering_launch_date and Re.course_id = r.session_course_id
        );
        return next;
    end loop;
    close curs;
end;

$$ Language plpgsql;

-- Function 17
-- register for a session
-- likewise for redeeeming, we must make sure that have an existing
-- package they can use.
create or replace procedure register_session(cust_id integer, launch_date date, course_id integer, sid integer, payment_method text)
as $$
declare
    date_of_transaction date;
    card_number text;
    package_buy_date date;
    redeem_package_id integer;
    input_id integer := cust_id;
begin
    select CURRENT_DATE into date_of_transaction;
    if payment_method = 'credit-card' then
    -- if customer wants to register for a session, we must make sure that
    -- own a card first.
        if exists (
            select * 
            from Owns O
            where O.cust_id = input_id
        )
        then 
        select number into card_number
        from Owns O
        where O.cust_id = input_id
        limit 1;

        insert into Registers (date, cust_id, number, sid, launch_date, course_id)
        values (date_of_transaction, cust_id, card_number, sid, launch_date, course_id);

        else 
            raise exception 'Customer has no valid credit cards to make this purchase';
        end if;

    elseif payment_method = 'redeem' then
    -- likewise for redeeeming, we must make sure that have an existing
    -- package they can use.
        if exists (
            select 1
            from Buys B 
            where B.cust_id = input_id
        )
        then
        select buy_date, package_id, number into package_buy_date, redeem_package_id, card_number
        from Buys B
        where B.cust_id = input_id;
        insert into Redeems (redeem_date, buy_date, cust_id, number, package_id, sid, launch_date, course_id) 
        values (date_of_transaction, package_buy_date, cust_id, card_number, redeem_package_id, sid, launch_date, course_id);
        insert into Registers (date, cust_id, number, sid, launch_date, course_id)
        values (date_of_transaction, cust_id, card_number, sid, launch_date, course_id);

        else 
            raise exception 'This customer has no available packages to redeem a sessions from';
        end if;
    
    else 
        raise exception 'type of payment_method must be one of credit-card or redeem';
    end if;
end;
$$ Language plpgsql;


-- Function 18
-- get my registrations
create or replace function get_my_registrations(input_id integer)
returns TABLE(course_name TEXT, course_fees NUMERIC, session_date DATE, 
start_hour integer, session_duration integer, instr_name TEXT)
as $$
    select C.title, O.fees, Ses.date as ses_date, Ses.start_time as start_hour, Ses.end_time - Ses.start_time, I.name
    from 
        Registers Reg join Sessions Ses on (Reg.sid = Ses.sid and Reg.launch_date = Ses.launch_date and Reg.course_id = Ses.course_id)
        join Offerings O on (O.launch_date = Ses.launch_date and O.course_id = Ses.course_id)
        join Courses C on (C.course_id = Ses.course_id) join Instructors I on (I.eid = Ses.eid)
    where Reg.cust_id = input_id and current_date < ses.date + ses.end_time
    order by ses_date, start_hour asc;
$$ Language SQL; 


-- Function 19
-- update course session
create or replace procedure update_course_session(cust_id integer, course_id integer, launch_date date, new_session_num integer)
as $$
declare 
    input_cust_id integer := cust_id;
    input_course_id integer := course_id;
    input_launch_date date := launch_date;
begin 
    if not exists (
        select 1
        from Registers Reg
        where Reg.cust_id = input_cust_id and Reg.course_id = input_course_id
        and Reg.launch_date = input_launch_date
    ) then
    raise exception 'Customer has not redeemed or registered for a session for the selected course offering';

    elseif not exists (
        select *
        from Sessions Ses 
        where Ses.sid = new_session_num and Ses.course_id = input_course_id 
        and Ses.launch_date = input_launch_date
    ) then
    raise exception 'New session to change to does not exist';
    else 
        update Registers Reg
        set sid = new_session_num
        where Reg.cust_id = input_cust_id and Reg.course_id = input_course_id
        and Reg.launch_date = input_launch_date;

    end if;
end;
$$ Language plpgsql;

-- Function 20
-- register with money: refund 90% of the paid fees for a registered course if the cancellation is made at
-- least 7 days before the day of the registered session
-- redeem: credit an extra course session to the customer’s course package if the cancellation is made at
-- least 7 days before the day of the registered session
CREATE OR REPLACE PROCEDURE cancel_registration(cust_id INTEGER, launch_date Date, course_id INTEGER)
AS $$
DECLARE
  session_id INTEGER;
  date_of_session Date;
  date_of_cancellation Date;
  fees NUMERIC;

BEGIN
  -- does not have a registered session
  IF (cust_id, launch_date, course_id) NOT IN (
    SELECT R.cust_id, R.launch_date, R.course_id
    FROM Registers R
  ) THEN
    RAISE EXCEPTION 'No registed session found';
  END IF;

  SELECT R.sid into session_id
  FROM Registers R
  WHERE R.cust_id = $1
  AND R.launch_date = $2
  AND R.course_id = $3;

  SELECT S.date into date_of_session
  FROM Sessions S
  WHERE S.sid = session_id
  AND S.launch_date = $2
  AND S.course_id = $3;

  SELECT CURRENT_DATE INTO date_of_cancellation;
  
  -- cust redeems a session from package
  IF (cust_id, session_id, launch_date, course_id) IN (
    SELECT R.cust_id, R.sid, R.launch_date, R.course_id
    FROM Redeems R
  ) THEN

      IF (cust_id, session_id, launch_date, course_id) IN (
        SELECT C.cust_id, C.sid, C.launch_date, C.course_id
        FROM Cancels C
      ) THEN
        RAISE EXCEPTION 'Session has already been cancelled!';
      END IF;

      IF date_of_cancellation + 7 <= date_of_session THEN -- credit extra course session
        -- update Cancels table
        INSERT INTO Cancels(cust_id, date, refund_amt, package_credit, sid, launch_date, course_id)
        VALUES (cust_id, date_of_cancellation, null, 1, session_id, launch_date, course_id);
        
      ELSEIF date_of_cancellation > date_of_session THEN -- cannot cancel after session is over
        RAISE EXCEPTION 'Cannot cancel after session is over!';
      
      ELSE -- credit = 0
        INSERT INTO Cancels(cust_id, date, refund_amt, package_credit, sid, launch_date, course_id)
        VALUES (cust_id, date_of_cancellation, null, 0, session_id, launch_date, course_id);
      
      END IF;
  
  -- cust registered for session with money
  ELSE

      SELECT S.date into date_of_session
      FROM Sessions S
      WHERE S.sid = session_id
      AND S.launch_date = $2
      AND S.course_id = $3;

      SELECT O.fees INTO fees
      FROM Offerings O
      WHERE O.launch_date = $2
      AND O.course_id = $3;

      IF date_of_cancellation + 7 <= date_of_session THEN -- refund amount = 90%
        INSERT INTO Cancels(cust_id, date, refund_amt, package_credit, sid, launch_date, course_id)
        VALUES (cust_id, date_of_cancellation, 0.9*fees, null, session_id, launch_date, course_id);

      ELSEIF date_of_cancellation > date_of_session THEN -- cannot cancel after session is over
        RAISE EXCEPTION 'Cannot cancel after session is over!';

      ELSE -- refund amount = 0
        INSERT INTO Cancels(cust_id, date, refund_amt, package_credit, sid, launch_date, course_id)
        VALUES (cust_id, date_of_cancellation, 0, null, session_id, launch_date, course_id);

      END IF;
      
  END IF;
END; 
$$ LANGUAGE plpgsql;

-- Function 21
-- course session havent start and req valid, then update
CREATE OR REPLACE PROCEDURE update_instructor(launch_date Date, course_id INTEGER, sid INTEGER, eid INTEGER)
AS $$
DECLARE
  date_of_session Date;
  time_of_session INTEGER;
  date_of_update Date;
  time_of_update INTEGER;

BEGIN
  SELECT date, start_time INTO date_of_session, time_of_session
  FROM Sessions S
  WHERE S.sid = $3
  AND S.launch_date = $1
  AND S.course_id = $2;

  SELECT CURRENT_DATE INTO date_of_update;
  SELECT EXTRACT(HOUR FROM CURRENT_TIMESTAMP) INTO time_of_update;

-- need to check instructor availability
  IF date_of_update = date_of_session THEN
    IF time_of_update < time_of_session THEN
      -- can update
      UPDATE Sessions S
      SET eid = $4
      WHERE S.sid = $3
      AND S.launch_date = $1
      AND S.course_id = $2;

    ELSE
      RAISE EXCEPTION 'same date but time of update after start time of session';
    
    END IF;
  
  ELSEIF date_of_update > date_of_session THEN
    RAISE EXCEPTION 'date of update after date of session';

  ELSE
    -- can update
    UPDATE Sessions S
    SET eid = $4
    WHERE S.sid = $3
    AND S.launch_date = $1
    AND S.course_id = $2;
  
  END IF;
END; 
$$ LANGUAGE plpgsql;

-- Function 22
CREATE OR REPLACE PROCEDURE update_room(launch_date Date, course_id INTEGER, sid INTEGER, rid INTEGER)
AS $$
DECLARE
  num_registered INTEGER;
  new_seating_capacity INTEGER;

  date_of_session Date;
  time_of_session INTEGER;
  date_of_update Date;
  time_of_update INTEGER;

BEGIN
  SELECT COUNT(*) into num_registered
  FROM Registers R
  WHERE R.launch_date = $1
  AND R.course_id = $2
  AND R.sid = $3;

  SELECT seating_capacity into new_seating_capacity
  FROM Rooms R
  WHERE R.rid = $4;

  SELECT date, start_time INTO date_of_session, time_of_session
  FROM Sessions S
  WHERE S.sid = $3
  AND S.launch_date = $1
  AND S.course_id = $2;

  SELECT CURRENT_DATE INTO date_of_update;
  SELECT EXTRACT(HOUR FROM CURRENT_TIMESTAMP) INTO time_of_update;

  IF (date_of_update = date_of_session AND time_of_update < time_of_session)
    OR (date_of_update < date_of_session) THEN
      IF num_registered > new_seating_capacity THEN
        -- cannot update
        RAISE EXCEPTION 'number of registrations exceed seating capacity of new room';
      ELSE
        -- can update
        UPDATE Sessions S
        SET rid = $4
        WHERE S.sid = $3
        AND S.launch_date = $1
        AND S.course_id = $2;
      END IF;
  
  ELSE
    RAISE EXCEPTION 'course session has started, cannot update';

  END IF;
END; 
$$ LANGUAGE plpgsql;

-- Function 23
CREATE OR REPLACE PROCEDURE remove_session(launch_date Date, course_id INTEGER, sid INTEGER)
AS $$
DECLARE
  num_registered INTEGER;

  date_of_session Date;
  time_of_session INTEGER;
  date_of_update Date;
  time_of_update INTEGER;

BEGIN
-- need check if have been cancelled?
  SELECT COUNT(*) into num_registered
  FROM Registers R
  WHERE R.launch_date = $1
  AND R.course_id = $2
  AND R.sid = $3;

  SELECT date, start_time INTO date_of_session, time_of_session
  FROM Sessions S
  WHERE S.sid = $3
  AND S.launch_date = $1
  AND S.course_id = $2;

  SELECT CURRENT_DATE INTO date_of_update;
  SELECT EXTRACT(HOUR FROM CURRENT_TIMESTAMP) INTO time_of_update;

  IF (date_of_update > date_of_session) OR (date_of_update = date_of_session AND time_of_update >= time_of_session) THEN
    RAISE EXCEPTION 'course session has started, cannot remove';
  ELSE
    IF num_registered >= 1 THEN
      -- cannot remove session
      RAISE EXCEPTION 'number of registrations >= 1, cannot remove session';
    ELSE
      -- can remove session
      -- do i need a trigger here? hmm
      DELETE FROM Sessions S
      WHERE S.launch_date = $1
      AND S.course_id = $2
      AND S.sid = $3;
    END IF;
  END IF;
END; 
$$ LANGUAGE plpgsql;

-- Function 24
CREATE OR REPLACE PROCEDURE add_session(launch_date Date, course_id INTEGER, sid INTEGER, date Date, start_time INTEGER, eid INTEGER, rid INTEGER)
AS $$
DECLARE
  registration_deadline Date;
  date_of_addition Date;

BEGIN
  SELECT O.registration_deadline INTO registration_deadline
  FROM Sessions S, Offerings O
  WHERE S.launch_date = O.launch_date
  AND S.course_id = O.course_id
  AND S.sid = $3;

  SELECT CURRENT_DATE INTO date_of_addition;

  IF date_of_addition > registration_deadline THEN
    RAISE EXCEPTION 'course offering registration deadline is over';
  ELSE
    -- can add session
    INSERT INTO Sessions(sid, date, end_time, start_time, launch_date, course_id, rid, eid)
    VALUES ($3, $4, null, $5, $1, $2, $7, $6);
  END IF;

END; 
$$ LANGUAGE plpgsql;

-- Function 25
-- 'part-time' num_work_days = null, monthly_rate = null
-- 'full-time' num_work_hours = null, hourly_rate = null
-- dk whether to insert null tuples?
CREATE OR REPLACE FUNCTION pay_salary()
RETURNS TABLE (
  eid INTEGER,
  name TEXT,
  status TEXT, -- 'part-time' or 'full-time'
  num_work_days INTEGER,
  num_work_hours INTEGER,
  hourly_rate NUMERIC,
  monthly_rate NUMERIC,
  salary_paid NUMERIC
) AS $$
DECLARE
  payment_date Date;
  payment_day INTEGER;
  payment_month INTEGER;
  payment_year INTEGER;
  last_day_of_month INTEGER;
  curs CURSOR FOR (
    SELECT *
    FROM Employees E
    ORDER BY E.eid ASC
  );

  r RECORD;
  add_hourly_rate INTEGER;
  add_monthly_rate INTEGER;
  add_hours_worked INTEGER;
  depart_day INTEGER;

BEGIN
  SELECT CURRENT_DATE INTO payment_date;
  SELECT EXTRACT(DAY FROM CURRENT_DATE) INTO payment_day;
  SELECT EXTRACT(MONTH FROM CURRENT_DATE) INTO payment_month;
  SELECT EXTRACT(YEAR FROM CURRENT_DATE) INTO payment_year;
  SELECT DATE_PART('days', DATE_TRUNC('month', CURRENT_TIMESTAMP) + '1 MONTH'::INTERVAL - '1 DAY'::INTERVAL) INTO last_day_of_month;

  OPEN curs;
  
  LOOP
    FETCH curs INTO r;
    EXIT WHEN NOT FOUND;

    IF payment_date < r.join_date THEN
      CONTINUE; -- don't have to pay before join
    
    -- part-time
    ELSEIF r.eid IN (SELECT P.eid FROM Part_time_Emp P) THEN
      SELECT COALESCE(SUM(end_time - start_time), 0) INTO add_hours_worked
      FROM Part_time_Emp P, Sessions S
      WHERE P.eid = S.eid
      AND P.eid = r.eid
      AND EXTRACT(YEAR FROM S.date) = payment_year
      AND EXTRACT(MONTH FROM S.date) = payment_month;

      SELECT P.hourly_rate INTO add_hourly_rate
      FROM Part_time_Emp P
      WHERE P.eid = r.eid;

      IF add_hours_worked IS NOT NULL THEN
        eid := r.eid;
        name := r.name;
        status := 'part-time';
        num_work_days := NULL;
        hourly_rate := add_hourly_rate;
        monthly_rate := NULL;
        salary_paid := (add_hours_worked * add_hourly_rate);
        RETURN NEXT;

        INSERT INTO Pay_slips(payment_date, amount, num_work_hours, num_work_days, eid)
        VALUES (payment_date, add_hours_worked * add_hourly_rate, add_hours_worked, NULL, r.eid);
      END IF;
    
    -- full-time
    ELSE

      SELECT F.monthly_rate INTO add_monthly_rate
      FROM Full_time_Emp F
      WHERE F.eid = r.eid;

      eid := r.eid;
      name := r.name;
      status := 'full-time';
      hourly_rate := NULL;
      monthly_rate := add_monthly_rate;
      
      IF r.depart_date IS NULL OR payment_date <= r.depart_date THEN -- pay
        IF payment_month = EXTRACT(MONTH FROM r.join_date) THEN -- joined this month
          IF (r.depart_date IS NOT NULL) AND (payment_month = EXTRACT(MONTH FROM r.depart_date)) THEN -- first month of work = last month of work
            -- first work day = join day
            -- last work day = depart day
            num_work_days := r.depart_date - r.join_date + 1;
            salary_paid := ((r.depart_date - r.join_date)/last_day_of_month) * add_monthly_rate;
            RETURN NEXT;

            INSERT INTO Pay_slips(payment_date, amount, num_work_hours, num_work_days, eid)
            VALUES (CURRENT_DATE, ((r.depart_date - r.join_date)/last_day_of_month) * add_monthly_rate, NULL, r.depart_date - r.join_date, r.eid);

          ELSE -- paying for first month of work, not leaving in this month
            -- first work day = join day
            -- last work day = last day of month
            num_work_days := last_day_of_month - EXTRACT(DAY FROM r.join_date) + 1;
            salary_paid := ((last_day_of_month - EXTRACT(DAY FROM r.join_date) + 1)/last_day_of_month) * add_monthly_rate;
            RETURN NEXT;

            INSERT INTO Pay_slips(payment_date, amount, num_work_hours, num_work_days, eid)
            VALUES (CURRENT_DATE, ((last_day_of_month - EXTRACT(DAY FROM r.join_date) + 1)/last_day_of_month) * add_monthly_rate, NULL, last_day_of_month - EXTRACT(DAY FROM r.join_date) + 1, r.eid);

          END IF;

        ELSEIF (r.depart_date IS NULL) OR payment_month < EXTRACT(MONTH FROM r.depart_date) THEN --didn't join and didn't depart
          -- first work day = 1
          -- last work day = last day of month
          num_work_days := last_day_of_month;
          salary_paid := add_monthly_rate;
          RETURN NEXT;

          INSERT INTO Pay_slips(payment_date, amount, num_work_hours, num_work_days, eid)
          VALUES (CURRENT_DATE, add_monthly_rate, NULL, last_day_of_month, r.eid);

        ELSE -- paying for last month of work, didn't join this month
          -- first work day = 1
          -- last work day = depart day
          SELECT EXTRACT(DAY FROM r.depart_date) INTO depart_day;
          num_work_days := depart_day; --wrong
          salary_paid := (r.depart_day/last_day_of_month) * add_monthly_rate;
          RETURN NEXT;

          INSERT INTO Pay_slips(payment_date, amount, num_work_hours, num_work_days, eid)
          VALUES (CURRENT_DATE, (r.depart_day/last_day_of_month) * add_monthly_rate, NULL, r.depart_day, r.eid);
        END IF;

      ELSEIF (r.depart_date IS NOT NULL) AND payment_date > r.depart_date AND payment_date >= r.join_date AND payment_month = EXTRACT(MONTH FROM r.depart_date) THEN -- pay the last month after departing
        -- first work day = 1
        -- last work day = depart day
        SELECT EXTRACT(DAY FROM r.depart_date) INTO depart_day;
        num_work_days := r.depart_day;
        salary_paid := (r.depart_day/last_day_of_month) * add_monthly_rate;
        RETURN NEXT;

        INSERT INTO Pay_slips(payment_date, amount, num_work_hours, num_work_days, eid)
        VALUES (CURRENT_DATE, (r.depart_day/last_day_of_month) * add_monthly_rate, NULL, r.depart_day, r.eid);

      END IF;

    END IF;
  END LOOP;
  CLOSE curs;

END; 
$$ LANGUAGE plpgsql;

-- Function 26
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

-- Function 27
-- top N packages
-- DESCENDING ORDER OF NUM OF PACKAGES
-- DESCENDING ORDER OF PRICE
CREATE OR REPLACE FUNCTION top_packages(IN N INTEGER)
RETURNS TABLE (
  package_id INTEGER,
  num_free_registrations INTEGER,
  price NUMERIC,
  sale_start_date Date,
  sale_end_date Date,
  num_of_packages_sold INTEGER
) AS $$
DECLARE
  curs CURSOR FOR (
    SELECT C.package_id, C.num_free_registrations, C.price, C.sale_start_date, C.sale_end_date, COUNT(B.cust_id) as num_of_cust
    FROM Buys B, Course_packages C
    WHERE B.package_id = C.package_id
    GROUP BY C.package_id
    ORDER BY COUNT(B.cust_id) desc, C.price desc
  );
  count INTEGER; -- keep count of number of pakages
  prev_package_num_of_cust INTEGER; -- keep track of prev package' sold amount so if same, can add even if count >= N
  r RECORD;

BEGIN

  prev_package_num_of_cust := -1;
  count := 0;
  OPEN curs;
  
  LOOP
    FETCH curs INTO r;
    EXIT WHEN NOT FOUND;

    -- exit loop when already have more than N records and the next record has lower sales
    EXIT WHEN count >= N AND r.num_of_cust <> prev_package_num_of_cust;

    package_id := r.package_id;
    num_free_registrations := r.num_free_registrations;
    price := r.price;
    sale_start_date := r.sale_start_date;
    sale_end_date := r.sale_end_date;
    num_of_packages_sold := r.num_of_cust;
    RETURN NEXT;
    count := count + 1;
    prev_package_num_of_cust := r.num_of_cust;
  END LOOP;
  CLOSE curs;
END; 
$$ LANGUAGE plpgsql;

-- Function 28
-- i cant test this LOL i need more data got a feeling its not very correct
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

-- Function 29
-- view summary report
create or replace function view_summary_report(num_months in integer)
returns TABLE(month date, total_salary NUMERIC, total_course_package_sales NUMERIC, total_registration_fees NUMERIC, total_refunded_amount NUMERIC,total_session_redemption integer)
as $$
declare

    counter_month integer;
    curr_date date;
    date_in_yyyyMM date;

begin
    counter_month := num_months;
    select current_date into curr_date;
    select date_trunc('month', curr_date) into date_in_yyyyMM;
    
    loop
        exit when counter_month = 0;
        month := date_in_yyyyMM;
        total_salary := coalesce(
            (
            select sum(amount)
            from Pay_slips
            where date_in_yyyyMM = (select date_trunc('month', payment_date))
        ), 0.0);
        total_course_package_sales := coalesce(
            (
            select sum(price)
            from Course_packages P join Buys B on (P.package_id = B.package_id)
            where date_in_yyyyMM = (select date_trunc('month', buy_date))
        ), 0);
        total_registration_fees := coalesce(
            (
            select sum(O.fees)
            from Registers R join Offerings O on (R.course_id = O.course_id and R.launch_date = O.launch_date)
            where date_in_yyyyMM = (select date_trunc('month', R.date))
        ), 0.0);
        total_refunded_amount := coalesce(
            (
            select sum(C.refund_amt)
            from Cancels C
            where date_in_yyyyMM = (select date_trunc('month', C.date))
        ), 0.0);
        total_session_redemption := coalesce(
            (
            select count(*)
            from Redeems R 
            where date_in_yyyyMM = (select date_trunc('month', R.redeem_date))
        ), 0);

        return next;

        date_in_yyyyMM := date_in_yyyyMM - interval '1 month';

        counter_month := counter_month - 1;
    end loop;
end; 

$$ Language plpgsql;


-- Function 30
-- Example call: 
-- select * from view_manager_report();
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

		RETURN NEXT;
		
	END LOOP;
	CLOSE report_curs;
		
END;
$$ LANGUAGE PLPGSQL;
