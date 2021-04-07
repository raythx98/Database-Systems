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
  CHECK (start_date <= end_date AND launch_date <= start_date),
  CHECK (registration_deadline >= launch_date AND registration_deadline <= end_date),
  CHECK (target_number_registrations <= seating_capacity),
  CHECK (registration_deadline <= start_date - 10)
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
