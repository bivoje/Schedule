
CREATE TABLE IF NOT EXISTS che_professor (
  name NVARCHAR(50) NOT NULL PRIMARY KEY,
  names NVARCHAR(200) NOT NULL
);

CREATE TABLE IF NOT EXISTS professor (
  name NVARCHAR(50) NOT NULL PRIMARY KEY,
  office CHAR(10) NULL,
  full TINYINT(1) NULL,
  email NVARCHAR(255) NULL
    -- ON DELETE 
    -- ON UPDATE 
);

CREATE TABLE IF NOT EXISTS ta (
  name NVARCHAR(50) NOT NULL PRIMARY KEY,
  email NVARCHAR(255) NULL
);


CREATE TABLE IF NOT EXISTS course (
  crs_id CHAR(6) NOT NULL PRIMARY KEY CHECK (
    (SUBSTRING(crs_id,1,2) IN
        ('GS','PS','CH','BS','EC','MC','MA','EV'))
    AND (SUBSTRING(crs_id,3) REGEXP '^[0-9]{4}$')
  ),
  -- classify CHAR(2) NOT NULL CHECK (classify IN ('EE')),
  title VARCHAR(100) NOT NULL,
  title_kr NVARCHAR(100) NOT NULL,
  credit TINYINT NOT NULL,
  -- semester = season (1|2|3|4) * 100 + year (last 2)
  -- 2017 Summer -> 2 * 100 + 17 = 217
  semester SMALLINT NOT NULL CHECK (100 <= semester AND semester < 500),
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
  -- prof/ta might not be determined by the time the list released
  prof NVARCHAR(50) NULL REFERENCES professor (name),
  ta NVARCHAR(50) NULL REFERENCES ta (name),
  room NVARCHAR(20) NULL,
  enroll_size SMALLINT NULL,
  CONSTRAINT PRIMARY KEY (crsid, sect_no)
);

CREATE TABLE IF NOT EXISTS class (
  crsid CHAR(6) NOT NULL,
  sectno TINYINT NOT NULL,
  day CHAR(3) NOT NULL CHECK (day IN ('MON','TUE','WED','THU','FRI','SAT','SUN')),
  period TINYINT NOT NULL,
  CONSTRAINT PRIMARY KEY (crsid, sectno, day, period),
  CONSTRAINT FOREIGN KEY (crsid,sectno) REFERENCES section (crsid,sect_no)
);
