CREATE TABLE room (
  room_id VARCHAR(10) NOT NULL PRIMARY KEY
  -- something to add?
);

CREATE TABLE professor (
  name NVARCHAR(50) NOT NULL PRIMARY KEY,
  roomid VARCHAR(10) NULL REFERENCES room (room_id)
    -- ON DELETE 
    -- ON UPDATE 
);

CREATE TABLE course (
  crs_id CHAR(6) NOT NULL PRIMARY KEY,
  classify CHAR(2) NOT NULL CHECK (classify IN ('EE')), -- yet to add more
  title VARCHAR(100) NOT NULL UNIQUE,
  title_kr NVARCHAR(100) NOT NULL UNIQUE,
  credit TINYINT NOT NULL,
  semester TINYINT NOT NULL CHECK (0 < semester AND semester < 5),
  requir CHAR(6) NULL REFERENCES course (crs_id)
);

CREATE TABLE section (
  sect_no TINYINT NOT NULL,
  crsid CHAR(6) NOT NULL REFERENCES course (crs_id),
  prof NVARCHAR(50) NOT NULL REFERENCES professor (name),
  room VARCHAR(10) NOT NULL REFERENCES room (room_id),
  CONSTRAINT PRIMARY KEY (crsid, sect_no)
);

CREATE TABLE class (
  sectno TINYINT NOT NULL REFERENCES section (sect_no),
  day CHAR(3) NOT NULL CHECK (day IN ('MON','TUE','WED','THU','FRI','SAT','SUN')),
  CONSTRAINT PRIMARY KEY (sectno, day)
);