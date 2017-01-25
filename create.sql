CREATE TABLE IF NOT EXISTS room (
  room_id VARCHAR(10) NOT NULL PRIMARY KEY
  -- something to add?
);

-- ASSUME this table for granted, since the infomation of professors not provided
CREATE TABLE IF NOT EXISTS professor (
  name NVARCHAR(50) NOT NULL PRIMARY KEY,
  roomid VARCHAR(10) NULL REFERENCES room (room_id),
  full TINYINT(1) NULL
    -- ON DELETE 
    -- ON UPDATE 
);

CREATE TABLE IF NOT EXISTS course (
  crs_id CHAR(6) NOT NULL PRIMARY KEY,
  school CHAR(4) NOT NULL CHECK (school IN ('EECS')), -- yet to add more
  classify CHAR(2) NOT NULL CHECK (classify IN ('EE')), -- yet to add more
  title VARCHAR(100) NOT NULL UNIQUE,
  title_kr NVARCHAR(100) NOT NULL UNIQUE,
  credit TINYINT NOT NULL,
  semester TINYINT NOT NULL CHECK (0 < semester AND semester < 5),
  /* requir 1, 2, 3 will be filled sequentially
     assume there be not more than 3 requir */
  requir1 CHAR(6) NULL REFERENCES course (crs_id),
  requir2 CHAR(6) NULL REFERENCES course (crs_id),
  requir3 CHAR(6) NULL REFERENCES course (crs_id),
  CHECK ((requir1 IS NOT NULL OR requir2 IS NULL)
     AND (requir2 IS NOT NULL OR requir3 IS NULL))
);

CREATE TABLE IF NOT EXISTS section (
  sect_no TINYINT NOT NULL,
  crsid CHAR(6) NOT NULL REFERENCES course (crs_id),
  prof NVARCHAR(50) NOT NULL REFERENCES professor (name),
  room VARCHAR(10) NOT NULL REFERENCES room (room_id),
  enroll_size SMALLINT NOT NULL,
  CONSTRAINT PRIMARY KEY (crsid, sect_no)
);

CREATE TABLE IF NOT EXISTS class (
  sectno TINYINT NOT NULL REFERENCES section (sect_no),
  day CHAR(3) NOT NULL CHECK (day IN ('MON','TUE','WED','THU','FRI','SAT','SUN')),
  CONSTRAINT PRIMARY KEY (sectno, day)
);
