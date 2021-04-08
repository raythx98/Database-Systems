DROP TABLE IF EXISTS Pay_slips, Redeems, Buys, Registers, Cancels, Sessions, Offerings, Course_packages, Courses, Rooms, Owns, Credit_cards,
Customers, Administrators, Full_time_instructors, Part_time_instructors, Instructors, Course_areas, Managers, Full_time_Emp, Part_time_Emp, Employees CASCADE;

CREATE TABLE Employees ( -- 1 to 100
  eid SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  phone INTEGER NOT NULL,
  email TEXT NOT NULL,
  join_date Date NOT NULL,
  address TEXT NOT NULL,
  depart_date Date,
  CHECK (join_date <= depart_date)
);

CREATE TABLE Part_time_Emp ( -- 1 to 50
  eid INTEGER PRIMARY KEY,
  hourly_rate NUMERIC NOT NULL,
  FOREIGN KEY(eid) REFERENCES Employees
    ON DELETE CASCADE,
  CHECK (hourly_rate > 0)
);

CREATE TABLE Full_time_Emp ( -- 51 to 100
  eid INTEGER PRIMARY KEY,
  monthly_rate NUMERIC NOT NULL,
  FOREIGN KEY(eid) REFERENCES Employees
    ON DELETE CASCADE,
  CHECK (monthly_rate > 0)
);

CREATE TABLE Managers ( -- 71 to 80
  eid INTEGER PRIMARY KEY,
  FOREIGN KEY(eid) REFERENCES Full_time_Emp
    ON DELETE CASCADE
);

CREATE TABLE Course_areas (
  name TEXT PRIMARY KEY,
  eid INTEGER NOT NULL REFERENCES Managers
);

CREATE TABLE Instructors ( -- 1 to 70
  eid INTEGER PRIMARY KEY,
  FOREIGN KEY(eid) REFERENCES Employees
    ON DELETE CASCADE,
  name TEXT NOT NULL REFERENCES Course_areas
);

CREATE TABLE Part_time_instructors (
  eid INTEGER PRIMARY KEY REFERENCES Part_time_Emp
    REFERENCES Instructors
    ON DELETE CASCADE
);

CREATE TABLE Full_time_instructors (
  eid INTEGER PRIMARY KEY REFERENCES Full_time_Emp
    REFERENCES Instructors
    ON DELETE CASCADE
);

CREATE TABLE Administrators ( -- 81 TO 100
  eid INTEGER PRIMARY KEY,
  FOREIGN KEY(eid) REFERENCES Full_time_Emp
    ON DELETE CASCADE
);

CREATE TABLE Customers (
  cust_id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  email TEXT,
  address TEXT,
  phone INTEGER
);

CREATE TABLE Credit_cards (
  number TEXT PRIMARY KEY,
  CVV INTEGER NOT NULL,
  expiry_date Date NOT NULL,
  cust_id INTEGER NOT NULL
    REFERENCES Customers
);

CREATE TABLE Owns (
  cust_id INTEGER REFERENCES Customers,
  number TEXT REFERENCES Credit_cards,
  from_date Date,
  PRIMARY KEY(cust_id, number)
);

CREATE TABLE Rooms (
  rid SERIAL PRIMARY KEY,
  location TEXT NOT NULL,
  seating_capacity INTEGER NOT NULL,
  CHECK (seating_capacity >= 0)
);

CREATE TABLE Courses (
  course_id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  duration NUMERIC NOT NULL,
  description TEXT,
  name TEXT NOT NULL REFERENCES Course_areas,
  CHECK (duration > 0)
);

CREATE TABLE Course_packages (
  package_id SERIAL PRIMARY KEY,
  sale_start_date Date,
  num_free_registrations INTEGER,
  name TEXT,
  sale_end_date Date,
  price NUMERIC,
  CHECK (sale_start_date <= sale_end_date), --KIV null pass check anot
  CHECK (price >= 0)
);

CREATE TABLE Offerings (
  launch_date Date,
  start_date Date,
  end_date Date,
  registration_deadline Date,
  target_number_registrations INTEGER CHECK (target_number_registrations >= 0),
  seating_capacity INTEGER CHECK (seating_capacity >= 0),
  fees NUMERIC CHECK (fees >= 0),
  eid INTEGER NOT NULL REFERENCES Administrators,
  course_id INTEGER REFERENCES Courses
    ON DELETE CASCADE,
  PRIMARY KEY(launch_date, course_id),
  CONSTRAINT a1 CHECK (start_date <= end_date AND launch_date <= start_date),
  CONSTRAINT b2 CHECK(registration_deadline >= launch_date AND registration_deadline <= end_date),
  CONSTRAINT c3 CHECK(target_number_registrations <= seating_capacity),
  CONSTRAINT d4 CHECK(registration_deadline = start_date - 10)
);

CREATE TABLE Sessions (
  sid INTEGER,
  date Date,
  end_time INTEGER,
  start_time INTEGER,
  launch_date Date,
  course_id INTEGER,
  rid INTEGER NOT NULL REFERENCES Rooms,
  eid INTEGER NOT NULL REFERENCES Instructors,
  PRIMARY KEY(sid, launch_date, course_id),
  FOREIGN KEY(launch_date, course_id) REFERENCES Offerings,
  CHECK (start_time < end_time),
  CHECK (launch_date <= date)
);

CREATE TABLE Cancels (
  cust_id INTEGER REFERENCES Customers,
  date Date, -- 7 days before session date, after registers date / redeems redeem_date.
  refund_amt NUMERIC CHECK (refund_amt >= 0), -- either refund_amt or package_credit must be null.
  package_credit INTEGER CHECK (package_credit in (0, 1)),
  sid INTEGER,
  launch_date Date,
  course_id INTEGER,
  FOREIGN KEY(sid, launch_date, course_id) REFERENCES Sessions,
  PRIMARY KEY(date, cust_id, sid, launch_date, course_id),
  CHECK (launch_date < date),
  CONSTRAINT null_check CHECK((refund_amt IS NOT null AND package_credit IS NULL) OR (refund_amt IS null AND package_credit IS NOT NULL))
);

CREATE TABLE Registers (
  date Date, -- after session launch_date, 10 days before offerings reg_deadline
  cust_id INTEGER,
  number TEXT,
  sid INTEGER,
  launch_date Date,
  course_id INTEGER,
  PRIMARY KEY(date, cust_id, number, sid, launch_date, course_id),
  FOREIGN KEY(cust_id, number) REFERENCES Owns,
  FOREIGN KEY(sid, launch_date, course_id) REFERENCES Sessions,
  CHECK (date >= launch_date)
);

CREATE TABLE Buys (
  buy_date Date, -- after course packages sale date, before sale end date
  num_remaining_redemptions INTEGER CHECK (num_remaining_redemptions >= 0), -- less than num_free_registrations Course_packages
  cust_id INTEGER, -- 16 to 30
  number TEXT,
  package_id INTEGER REFERENCES Course_packages,
  PRIMARY KEY(buy_date, cust_id, number, package_id),
  FOREIGN KEY(cust_id, number) REFERENCES Owns
);

CREATE TABLE Redeems (
  redeem_date Date, -- after buy_date
  buy_date Date, -- after course packages sale date
  cust_id INTEGER, 
  number TEXT,
  package_id INTEGER,
  sid INTEGER,
  launch_date Date,
  course_id INTEGER,
  FOREIGN KEY(buy_date, cust_id, number, package_id) REFERENCES Buys,
  FOREIGN KEY(sid, launch_date, course_id) REFERENCES Sessions,
  PRIMARY KEY(redeem_date, buy_date, cust_id, number, package_id, sid, launch_date, course_id),
  CHECK (redeem_date >= buy_date)
);

CREATE TABLE Pay_slips (
  payment_date Date,
  amount NUMERIC NOT NULL CHECK (amount >= 0),
  num_work_hours NUMERIC CHECK (num_work_hours >= 0),
  num_work_days NUMERIC CHECK (num_work_days >= 0),
  eid INTEGER REFERENCES Employees
    ON DELETE CASCADE,
  PRIMARY KEY(payment_date, eid),
  CONSTRAINT null_check CHECK((num_work_hours IS NOT null AND num_work_days IS NULL) OR (num_work_hours IS null AND num_work_days IS NOT NULL))
);












-- ####################################
-- ####          TRIGGERS          ####
-- ####################################

CREATE TRIGGER Owns_insert_trigger
BEFORE INSERT ON Buys
FOR EACH ROW EXECUTE FUNCTION check_for_owns_insert();

CREATE OR REPLACE FUNCTION check_for_owns_insert() RETURNS TRIGGER AS $$
BEGIN

    IF (NEW.cust_id not in (SELECT cust_id FROM Customers)) THEN
        RAISE EXCEPTION 'Customer does not exist';
        RETURN NULL;
    END IF;

    IF (NEW.number not in (SELECT number FROM Credit_cards)) THEN
        RAISE EXCEPTION 'Card does not exist';
        RETURN NULL;
    END IF;

    IF (NEW.package_id not in (SELECT cp.package_id FROM Course_packages cp)) THEN
        RAISE EXCEPTION 'Package does not exist';
        RETURN NULL;
    END IF;

    IF (NEW.cust_id not in (SELECT cust_id FROM Owns)) THEN
        RAISE EXCEPTION 'Customer does not exist';
        RETURN NULL;
    END IF;

    IF (NEW.num_remaining_redemptions > (SELECT num_free_registrations FROM Course_packages cp WHERE NEW.package_id = cp.package_id)) THEN
        RAISE EXCEPTION 'Number of remaining redemption must be <= initial';
        RETURN NULL;
    END IF;

    IF (NEW.buy_date > (SELECT expiry_date FROM Credit_cards C WHERE NEW.number = C.number)) THEN
        RAISE EXCEPTION 'Credit card has expired';
        RETURN NULL;
    END IF;

    IF (NEW.buy_date > (SELECT sale_end_date FROM Course_packages cp WHERE NEW.package_id = cp.package_id)) THEN
        RAISE EXCEPTION 'Sale of package has ended';
        RETURN NULL;
    END IF;

    IF (NEW.buy_date < (SELECT sale_start_date FROM Course_packages cp WHERE NEW.package_id = cp.package_id)) THEN
        RAISE EXCEPTION 'Sale of package has yet to start';
        RETURN NULL;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER Owns_update_trigger
BEFORE UPDATE ON Buys
FOR EACH ROW EXECUTE FUNCTION check_for_owns_update();

CREATE OR REPLACE FUNCTION check_for_owns_update() RETURNS TRIGGER AS $$
BEGIN

    IF (NEW.cust_id <> OLD.cust_id) THEN
        RAISE EXCEPTION 'Cannot change customer/owner';
        RETURN NULL;
    END IF;

    IF (NEW.number <> OLD.number) THEN
        RAISE EXCEPTION 'Cannot change card';
        RETURN NULL;
    END IF;

    IF (NEW.package_id <> OLD.package_id) THEN
        RAISE EXCEPTION 'Cannot change package';
        RETURN NULL;
    END IF;

    IF (NEW.buy_date <> OLD.buy_date) THEN
        RAISE EXCEPTION 'Buy date should not be updated';
        RETURN NULL;
    END IF;

    IF (NEW.num_remaining_redemptions > (SELECT num_free_registrations FROM Course_packages cp WHERE NEW.package_id = cp.package_id)) THEN
        RAISE EXCEPTION 'Number of remaining redemption must be <= initial';
        RETURN NULL;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER Owns_check_active
BEFORE INSERT ON Buys
FOR EACH ROW EXECUTE FUNCTION check_for_active_package();

CREATE OR REPLACE FUNCTION check_for_active_package() RETURNS TRIGGER AS $$
BEGIN

    IF (SELECT EXISTS (SELECT 1 FROM Buys B WHERE (B.cust_id = NEW.cust_id) and (B.number = NEW.number) and (B.num_remaining_redemptions > 0))) THEN
        RAISE EXCEPTION 'Customer has an existing active package';
        RETURN NULL;
    END IF;

    IF (SELECT EXISTS (SELECT 1 FROM (Redeems natural join Sessions) as RS WHERE (RS.cust_id = NEW.cust_id) and (RS.number = NEW.number) and (RS.date >= NEW.buy_date + 7))) THEN
        RAISE EXCEPTION 'Customer has an existing partially active package';
        RETURN NULL;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER Owns_delete_trigger
BEFORE DELETE ON Buys
FOR EACH ROW EXECUTE FUNCTION check_for_owns_delete();

CREATE OR REPLACE FUNCTION check_for_owns_delete() RETURNS TRIGGER AS $$
BEGIN
    RAISE EXCEPTION 'Entries should not be deleted for archival purposes';
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER Offerings_insert_trigger
BEFORE INSERT ON Offerings
FOR EACH ROW EXECUTE FUNCTION check_for_offerings_insert();

CREATE OR REPLACE FUNCTION check_for_offerings_insert() RETURNS TRIGGER AS $$
BEGIN

    IF (SELECT EXISTS (SELECT 1 FROM Offerings O WHERE (O.launch_date = NEW.launch_date) and (O.course_id = NEW.course_id))) THEN
        RAISE EXCEPTION 'Course offerings with same course id must have different launch date';
        RETURN NULL;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER Offerings_update_trigger
AFTER UPDATE ON Offerings
FOR EACH ROW EXECUTE FUNCTION check_for_offerings_update();

CREATE OR REPLACE FUNCTION check_for_offerings_update() RETURNS TRIGGER AS $$
BEGIN

    IF ((SELECT COUNT(*) FROM Offerings O WHERE (O.launch_date = NEW.launch_date) and (O.course_id = NEW.course_id)) > 1) THEN
        RAISE EXCEPTION 'Course offerings with same course id must have different launch date';
        RETURN NULL;
    END IF;

    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE CONSTRAINT TRIGGER match_offerings_sessions
AFTER INSERT OR UPDATE ON Offerings
DEFERRABLE INITIALLY DEFERRED
FOR EACH ROW EXECUTE FUNCTION check_for_offerings_insert_deferrable();

CREATE OR REPLACE FUNCTION check_for_offerings_insert_deferrable() RETURNS TRIGGER AS $$
BEGIN

    IF (NEW.seating_capacity <> (SELECT COALESCE(SUM(SR.seating_capacity), 0) FROM (Sessions natural join Rooms) as SR WHERE SR.launch_date = NEW.launch_date and SR.course_id = NEW.course_id)) THEN
        RAISE EXCEPTION 'Seating capacity does not correspond to room capacity of sessions, launch date: %, course id: %', NEW.launch_date, NEW.course_id;
        RETURN NULL;
    END IF;

    IF (SELECT NOT EXISTS (SELECT 1 FROM Sessions S WHERE S.launch_date = NEW.launch_date and S.course_id = NEW.course_id)) THEN
        RAISE EXCEPTION 'Offerings should contain at least 1 session';
        RETURN NULL;
    END IF;

    IF (NEW.start_date <> (SELECT COALESCE(min(date), date'1000-01-01') FROM Sessions S WHERE S.launch_date = NEW.launch_date and S.course_id = NEW.course_id)) THEN
        RAISE EXCEPTION 'Start date does not correspond to earliest session, launch date: %, course id: %', NEW.launch_date, NEW.course_id;
        RETURN NULL;
    END IF;

    IF (NEW.end_date <> (SELECT COALESCE(max(date), date'1000-01-01') FROM Sessions S WHERE S.launch_date = NEW.launch_date and S.course_id = NEW.course_id)) THEN
        RAISE EXCEPTION 'End date does not correspond to latest session, launch date: %, course id: %', NEW.launch_date, NEW.course_id;
        RETURN NULL;
    END IF;

    RETURN NULL;

END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER Offerings_delete_trigger
BEFORE DELETE ON Offerings
FOR EACH ROW EXECUTE FUNCTION check_for_offerings_delete();

CREATE OR REPLACE FUNCTION check_for_offerings_delete() RETURNS TRIGGER AS $$
BEGIN
    IF (SELECT NOT EXISTS (SELECT 1 FROM Sessions S WHERE S.launch_date = OLD.launch_date and S.course_id = OLD.course_id)) THEN
        RETURN OLD;
    END IF;
    RAISE EXCEPTION 'There is still 1 session under this course offering';
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;

create TRIGGER check_update_sessions_capacity
before update
on Registers
for each row 
execute function check_update_ses_capacity();

create or replace function check_update_ses_capacity()
returns trigger as
$$
declare
    session_capacity integer;
    seats_taken integer;
begin
    select seating_capacity into session_capacity
    from Sessions Ses join Rooms R on Ses.rid = R.rid
    where Ses.sid = new.sid and Ses.course_id = new.course_id
    and Ses.launch_date = new.launch_date;

    select count(*) into seats_taken
    from Registers R
    where (R.sid = new.sid and R.course_id = new.course_id and R.launch_date = new.launch_date);

    if seats_taken + 1 > session_capacity then 
        raise exception 'No more seats left for the selected session';
        return null;
    end if;

    return new;

end;
$$ Language plpgsql;


create TRIGGER check_update_sessions_date
before update
on Registers
for each row 
execute function check_update_ses_date();


create or replace function check_update_ses_date()
returns trigger as
$$
declare
    session_date date;
begin
    select S.date into session_date
    from Sessions S
    where S.sid = new.sid and S.course_id = new.course_id and S.launch_date = new.launch_date;

    if session_date < new.date then 
        raise exception 'Cannot change session to a session that is already over!';
        return null;
    end if;

    return new;

end;
$$ Language plpgsql;


create TRIGGER check_transaction_for_register_already_registered
before insert 
on Registers
for each row
execute function check_register_duplicate_registration();


create or replace function check_register_duplicate_registration()
returns trigger as
$$    
begin
    if exists (
        select 1
        from Registers
        where cust_id = new.cust_id and number = new.number and sid = new.sid
        and launch_date = new.launch_date and course_id = new.course_id
    ) 
    then raise exception 'Already registered for course on an earlier date';
    return null;
    
    end if;

    if exists (
        select 1
        from Registers
        where cust_id = new.cust_id and number = new.number and launch_date = new.launch_date and course_id = new.course_id
    ) 
    then raise exception 'Already registered for a session from the same course offering';
    return null;
    
    end if;
        
    return new;

end;
$$ Language plpgsql;



create TRIGGER check_transaction_for_register_capacity
before insert 
on Registers
for each row
execute function check_register_capacity();

create or replace function check_register_capacity()
returns trigger as
$$
declare 
    session_capacity integer;
    seats_filled integer;

begin
    select R.seating_capacity into session_capacity
    from Rooms R join Sessions S on (R.rid = S.rid)
    where (S.sid = new.sid and S.course_id = new.course_id and S.launch_date = new.launch_date);

    select count(*) into seats_filled
    from Registers R
    where (R.sid = new.sid and R.course_id = new.course_id and R.launch_date = new.launch_date);

    if seats_filled + 1 > session_capacity then 
        raise exception 'Session is fully subscribed, no more available seats';
        return null;
    end if;

    return new;
end;
$$ Language plpgsql;



create TRIGGER check_transaction_for_register_deadline
before insert 
on Registers
for each row
execute function check_register_deadline();


create or replace function check_register_deadline()
returns trigger as
$$
declare
    offering_reg_deadline date;
begin
    select registration_deadline into offering_reg_deadline
    from Offerings O
    where O.launch_date = new.launch_date and O.course_id = new.course_id;

    if new.date > offering_reg_deadline  then
        raise exception 'Can only register for a session 10 days before its registration deadline';
        return null;
    end if;
    return new;
end;
$$ Language plpgsql;


create TRIGGER check_transaction_for_redeem_balance
before insert 
on Redeems
for each row
execute function check_redeem_balance();

create or replace function check_redeem_balance()
returns trigger as
$$
declare 
    redemptions_left integer;

begin
    select num_remaining_redemptions into redemptions_left
    from Buys
    where cust_id = new.cust_id;

    if redemptions_left > 0 then
        update Buys B
        set num_remaining_redemptions = redemptions_left - 1
        where B.buy_date = new.buy_date and B.cust_id = new.cust_id and
        B.number = new.number and B.package_id = new.package_id;

        return new;
    
    else
        raise exception 'No more redemptions left';
        return null;
    end if;

end;
$$ Language plpgsql;


create TRIGGER check_transaction_for_redeem_date
before insert 
on Redeems
for each row
execute function check_redeem_date();

create or replace function check_redeem_date()
returns trigger as
$$
declare 
    ses_start_date date;
    reg_deadline date;
    redemptions_left integer;

begin
    select num_remaining_redemptions into redemptions_left
    from Buys
    where cust_id = new.cust_id;

    select S.date into ses_start_date 
    from Sessions S 
    where S.sid = new.sid and S.course_id = new.course_id and S.launch_date = new.launch_date;

    select O.registration_deadline into reg_deadline
    from Offerings O
    where O.launch_date = new.launch_date and O.course_id = new.course_id;

    if new.redeem_date > ses_start_date then
        raise exception 'Cannot redeem for a session that is already over';
        return null;

    elseif new.redeem_date > reg_deadline then
        raise exception 'Cannot redeem for a session 10 days before the offering registration deadline';
        return null;
    
    else
        return new;
    end if;

end;
$$ Language plpgsql;


create TRIGGER check_transaction_for_redeem_date
before insert 
on Redeems
for each row
execute function check_redeem_date();


CREATE OR REPLACE FUNCTION redeems_trigger_func() RETURNS TRIGGER AS $$ 
BEGIN
    IF (NEW.redeem_date > (SELECT date FROM Sessions s WHERE s.sid = NEW.sid AND s.launch_date = NEW.launch_date AND s.course_id = NEW.course_id)) THEN -- redeem after session
        raise exception 'Redeem date is after session date.';
        RETURN NULL; -- dont insert

    ELSEIF (NEW.buy_date < (SELECT sale_start_date FROM Course_packages cp where cp.package_id = NEW.package_id) 
    or NEW.buy_date > (SELECT sale_end_date FROM Course_packages cp where cp.package_id = NEW.package_id)) THEN -- i can delete this right
        raise exception 'Buy date is not during course package sales';
        RETURN NULL; -- dont insert
    ELSE
        INSERT INTO Registers (date, cust_id, number, sid, launch_date, course_id) 
        values (NEW.redeem_date, NEW.cust_id, NEW.number, NEW.sid, NEW.launch_date, NEW.course_id);
        RETURN NEW;
    END IF;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER redeems_trigger
BEFORE INSERT ON Redeems
FOR EACH ROW EXECUTE FUNCTION redeems_trigger_func();


CREATE TRIGGER for_Pay_slips_trigger
BEFORE INSERT OR UPDATE ON Pay_slips
FOR EACH ROW EXECUTE FUNCTION check_for_payslip();

CREATE OR REPLACE FUNCTION check_for_payslip() RETURNS TRIGGER AS $$
DECLARE
  join_date DATE;
  depart_date DATE;
BEGIN
  IF NEW.eid NOT IN (
    SELECT eid
    FROM Employees
  ) THEN
    RAISE exception 'employee does not exist';
    RETURN NULL;
  ELSE
    select E.join_date, E.depart_date into join_date, depart_date
    from Employees E
    where eid =  new.eid;

    IF new.payment_date < join_date or new.payment_date > depart_date then 
      RAISE exception 'cannot pay before join and cannot pay after depart';
      RETURN NULL;
    ELSEIF NEW.num_work_days > (NEW.payment_date - join_date) then 
      RAISE exception 'cannot add';
      RETURN NULL;
    END IF;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER for_Part_time_Emp_trigger
BEFORE INSERT OR UPDATE ON Part_time_Emp
FOR EACH ROW EXECUTE FUNCTION check_for_part_time_emp();

CREATE OR REPLACE FUNCTION check_for_part_time_emp() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.eid IN (
    SELECT eid
    FROM Full_time_Emp
  ) THEN
    RAISE exception 'already a full time emp';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER for_Full_time_Emp_trigger
BEFORE INSERT OR UPDATE ON Full_time_Emp
FOR EACH ROW EXECUTE FUNCTION check_for_full_time_emp();

CREATE OR REPLACE FUNCTION check_for_full_time_emp() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.eid IN (
    SELECT eid
    FROM Part_time_Emp
  ) THEN
    RAISE exception 'already a part time emp';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER for_Managers_ISA
BEFORE INSERT OR UPDATE ON Managers
FOR EACH ROW EXECUTE FUNCTION check_Managers();

CREATE OR REPLACE FUNCTION check_Managers() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.eid IN (
    SELECT eid
    FROM Administrators
  ) OR NEW.eid IN (
    SELECT eid
    FROM Full_time_instructors
  ) THEN
    RAISE exception 'already an administrator or full time instructor';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER for_Administrators_ISA
BEFORE INSERT OR UPDATE ON Administrators
FOR EACH ROW EXECUTE FUNCTION check_Administrators();

CREATE OR REPLACE FUNCTION check_Administrators() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.eid IN (
    SELECT eid
    FROM Managers
  ) OR NEW.eid IN (
    SELECT eid
    FROM Full_time_instructors
  ) THEN
    RAISE exception 'already a manager or full time instructor';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER for_Full_time_instructors
BEFORE INSERT OR UPDATE ON Full_time_instructors
FOR EACH ROW EXECUTE FUNCTION check_Full_time_instructors();

CREATE OR REPLACE FUNCTION check_Full_time_instructors() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.eid IN (
    SELECT eid
    FROM Managers
  ) OR NEW.eid IN (
    SELECT eid
    FROM Administrators
  ) OR NEW.eid IN (
    SELECT eid
    FROM Part_time_instructors
  ) THEN
    RAISE exception 'already a manager, administrator or part time instructor';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER for_Part_time_instructors
BEFORE INSERT OR UPDATE ON Part_time_instructors
FOR EACH ROW EXECUTE FUNCTION check_Part_time_instructors();

CREATE OR REPLACE FUNCTION check_Part_time_instructors() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.eid IN (
    SELECT eid
    FROM Full_time_instructors
  ) THEN
    RAISE exception 'already a full time instructor';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER for_Owns
BEFORE INSERT OR UPDATE ON Owns
FOR EACH ROW EXECUTE FUNCTION check_Owns();

CREATE OR REPLACE FUNCTION check_Owns() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.from_date > (
    SELECT expiry_date
    FROM Credit_cards
    WHERE NEW.cust_id = cust_id
  ) THEN
    RAISE exception 'card expired already';
    RETURN NULL;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;



-- amelia
-- drop trigger if exists for_sessions on sessions;
-- session start time and end time and rid cannot overlap i.e. room cannot be used for another session.
CREATE TRIGGER for_Sessions
BEFORE INSERT OR UPDATE ON Sessions
FOR EACH ROW EXECUTE FUNCTION check_Sessions();

CREATE OR REPLACE FUNCTION check_Sessions() RETURNS TRIGGER AS $$
DECLARE
  total_hours_taught INTEGER;
  total_sessions INTEGER;
  num_sessions_satisfy_constraint INTEGER;
BEGIN
  DROP TABLE IF EXISTS StartTimeTable, EndTimeTable;

  CREATE TABLE StartTimeTable AS ( -- all the start times except the current one
    SELECT start_time
    FROM Sessions
    WHERE NEW.rid = rid AND NEW.date = date
    AND (NEW.sid <> sid AND NEW.launch_date <> launch_date AND NEW.course_id <> course_id)
  );

  CREATE TABLE EndTimeTable AS (
    SELECT end_time
    FROM Sessions
    WHERE NEW.rid = rid AND NEW.date = date
    AND (NEW.sid <> sid AND NEW.launch_date <> launch_date AND NEW.course_id <> course_id)
  );

  -- Instructor must be specialised in the area
  IF NEW.eid NOT IN (
    SELECT I.eid
    FROM Courses C, Instructors I
    WHERE C.name = I.name
    AND C.course_id = NEW.course_id
  ) THEN
    RAISE exception 'Instructor is not specialized in that course area';
    RETURN NULL;
  END IF;

  -- duration must match Courses table
  IF (NEW.end_time - NEW.start_time) <> (
    SELECT C.duration
    FROM Courses C
    WHERE C.course_id = NEW.course_id
  ) THEN
    RAISE exception 'duration of session must match duration of course';
    RETURN NULL;
  END IF;

  -- Instructor must still be with the company during the session date.
  IF NEW.eid NOT IN (
    SELECT E.eid
    FROM Employees E
    WHERE NEW.date >= E.join_date
    AND (NEW.date <= E.depart_date OR E.depart_date IS NULL)
  ) THEN
    RAISE exception 'Instructor is no longer an employee during the session date';
    RETURN NULL;
  END IF;

  -- Each part-time instructor must not teach more than 30 hours for each month.
  IF NEW.eid IN (SELECT eid FROM Part_time_instructors) THEN
    SELECT SUM(end_time - start_time) INTO total_hours_taught
    FROM Part_time_instructors P, Sessions S
    WHERE P.eid = S.eid
    AND P.eid = NEW.eid
    AND (SELECT EXTRACT(MONTH FROM S.date)) = (SELECT EXTRACT(MONTH FROM CURRENT_DATE));

    IF total_hours_taught = 30 OR (total_hours_taught + (NEW.end_time - NEW.start_time) >= 30) THEN
      RAISE exception 'Each part-time instructor must not teach more than 30 hours for each month.';
      RETURN NULL;
    END IF;
  END IF;

  -- Instructor must be available during the time period. and
  -- An instructor cannot be assigned to teach two consecutive sessions.
  SELECT COUNT(*) INTO total_sessions -- total number of sessions
  FROM Sessions S
  WHERE S.eid = NEW.eid
  AND S.date = NEW.date
  AND (NEW.sid <> sid AND NEW.launch_date <> launch_date AND NEW.course_id <> course_id); -- remove, -1 instead
  
  SELECT COUNT(*) INTO num_sessions_satisfy_constraint -- number of sessions that satisfy constraint
  FROM Sessions S
  WHERE S.eid = NEW.eid
  AND S.date = NEW.date
  AND (NEW.sid <> sid AND NEW.launch_date <> launch_date AND NEW.course_id <> course_id)
  AND ((NEW.start_time - 1 >= S.start_time AND NEW.end_time >= S.end_time)
  OR (NEW.start_time <= S.start_time AND NEW.end_time + 1 <= S.end_time));

  IF total_sessions - num_sessions_satisfy_constraint > 0 THEN
    RAISE exception 'Instructor must be available during the time period and cannot teach 2 consecutive sessions.';
    RETURN NULL;
  END IF;

  -- Room can only hold one session at any one time.
  IF (NEW.date, NEW.rid) NOT IN ( -- room not occupied on that day
    SELECT date, rid
    FROM Sessions
  ) THEN
    -- update offerings start date & deadline
    IF NEW.date < (
      SELECT start_date
      FROM Offerings
      WHERE launch_date = NEW.launch_date
      AND course_id = NEW.course_id
    ) THEN
      UPDATE Offerings
      SET start_date = NEW.date, registration_deadline = NEW.date - 10
      WHERE launch_date = NEW.launch_date
      AND course_id = NEW.course_id;
    END IF;
    
    -- update offerings end date
    IF NEW.date > (
      SELECT end_date
      FROM Offerings
      WHERE launch_date = NEW.launch_date
      AND course_id = NEW.course_id
    ) THEN
      UPDATE Offerings
      SET end_date = NEW.date
      WHERE launch_date = NEW.launch_date
      AND course_id = NEW.course_id;
    END IF;

    RETURN NEW;
  ELSE
    IF NOT (NEW.start_time < ALL (SELECT start_time FROM StartTimeTable) AND NEW.end_time < ALL (SELECT start_time FROM StartTimeTable)) OR
      NOT (NEW.start_time > ALL (SELECT end_time FROM EndTimeTable) AND NEW.end_time > ALL (SELECT end_time FROM EndTimeTable)) THEN
      RAISE exception 'room is occupied at that time';
      RETURN NULL;
    ELSE -- room used after this session
      -- update offerings start date
      IF NEW.date < (
        SELECT start_date
        FROM Offerings
        WHERE launch_date = NEW.launch_date
        AND course_id = NEW.course_id
      ) THEN
        UPDATE Offerings
        SET start_date = NEW.date, registration_deadline = NEW.date - 10
        WHERE launch_date = NEW.launch_date
        AND course_id = NEW.course_id;
      END IF;
      
      -- update offerings end date
      IF NEW.date > (
        SELECT end_date
        FROM Offerings
        WHERE launch_date = NEW.launch_date
        AND course_id = NEW.course_id
      ) THEN
        UPDATE Offerings
        SET end_date = NEW.date
        WHERE launch_date = NEW.launch_date
        AND course_id = NEW.course_id;
      END IF;

      RETURN NEW; -- need to change offerings
    END IF;
  END IF;
END;
$$ LANGUAGE plpgsql;


-- drop trigger if exists for_session_deletion on sessions;
CREATE TRIGGER for_session_deletion
AFTER DELETE ON Sessions
FOR EACH ROW EXECUTE FUNCTION update_sessions_on_deletion();

CREATE OR REPLACE FUNCTION update_sessions_on_deletion() RETURNS TRIGGER AS $$
DECLARE
  offering_start_date Date;
  offering_end_date Date;
  num_of_sessions INTEGER;

  earliest_session Date;
  latest_session Date;

  target_reg INTEGER;
  capacity INTEGER;
BEGIN
  SELECT COUNT(*) INTO num_of_sessions
  FROM Sessions S
  WHERE S.launch_date = OLD.launch_date
  AND S.course_id = OLD.course_id;

  SELECT start_date, end_date INTO offering_start_date, offering_end_date
  FROM Offerings O
  WHERE O.launch_date = OLD.launch_date
  AND O.course_id = OLD.course_id;

  IF num_of_sessions = 0 THEN -- last session already, if delete this session must delete offering too.
    DELETE FROM Offerings
    WHERE launch_date = OLD.launch_date
    AND course_id = OLD.course_id;
  END IF;

  -- if start_date = s.date, change start date to next earliest date
  IF offering_start_date = OLD.date THEN
    SELECT date INTO earliest_session -- new earliest session
    FROM Sessions S
    WHERE S.launch_date = OLD.launch_date
    AND S.course_id = OLD.course_id
    AND S.sid <> OLD.sid
    ORDER BY date ASC
    LIMIT 1;

    UPDATE Offerings O
    SET start_date = earliest_session
    WHERE launch_date = OLD.launch_date
    AND course_id = OLD.course_id;
  END IF;

  -- if end_date = s.date, change end date to next latest date
  IF offering_end_date = OLD.date THEN
    SELECT date INTO latest_session -- new latest session
    FROM Sessions S
    WHERE S.launch_date = OLD.launch_date
    AND S.course_id = OLD.course_id
    AND S.sid <> OLD.sid
    ORDER BY date DESC
    LIMIT 1;

    UPDATE Offerings
    SET end_date = latest_session
    WHERE launch_date = OLD.launch_date
    AND course_id = OLD.course_id;
  END IF;

  SELECT seating_capacity INTO capacity
  FROM Offerings O
  WHERE O.launch_date = OLD.launch_date
  AND O.course_id = OLD.course_id;

  SELECT target_number_registrations INTO target_reg
  FROM Offerings O
  WHERE O.launch_date = OLD.launch_date
  AND O.course_id = OLD.course_id;

  -- if new seating capacity is lesser than target, decrease target.
  IF (capacity - (SELECT seating_capacity FROM Rooms R WHERE R.rid = OLD.rid)) < target_reg THEN
    UPDATE Offerings
    SET target_number_registrations = capacity
    WHERE launch_date = OLD.launch_date
    AND course_id = OLD.course_id;
  END IF;

  -- change seating capacity = original - rid.capacity
  UPDATE Offerings
  SET seating_capacity = seating_capacity - (SELECT seating_capacity FROM Rooms R WHERE R.rid = OLD.rid)
  WHERE launch_date = OLD.launch_date
  AND course_id = OLD.course_id;

  RETURN OLD;

END;
$$ LANGUAGE plpgsql;


CREATE TRIGGER for_Cancels
BEFORE INSERT OR UPDATE ON Cancels
FOR EACH ROW EXECUTE FUNCTION check_Cancels();

CREATE OR REPLACE FUNCTION check_Cancels() RETURNS TRIGGER AS $$
BEGIN
  IF NEW.date > ALL (
    SELECT date
    FROM Sessions
    WHERE NEW.sid = sid
    AND NEW.launch_date = launch_date
    AND NEW.course_id = course_id
  ) THEN
    RAISE exception 'cannot cancel after session';
    RETURN NULL;
  ELSEIF NEW.date < ALL (
    SELECT date
    FROM Registers
    WHERE NEW.cust_id = cust_id
    AND NEW.sid = sid
    AND NEW.launch_date = launch_date
    AND NEW.course_id = course_id
  ) THEN
    RAISE exception 'cannot cancel before registering %', NEW;
    RETURN NULL;
  ELSEIF NEW.package_credit = 1 THEN -- update Buys table
    UPDATE Buys B
    SET num_remaining_redemptions = num_remaining_redemptions + 1
    WHERE B.cust_id = NEW.cust_id;

    RETURN NEW;
  END IF;
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
