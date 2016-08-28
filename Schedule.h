#include <fstream>
#include <vector>
#include <set>

using namespace std;

//TODO not supported yet
enum classify { PEE };

//lecture times on timetable
enum lectime {
  MON1, MON2, MON3, MON4, MON5, MON6, MON7,
  TUE1, TUE2, TUE3, TUE4, TUE5, TUE6, TUE7,
  WED1, WED2, WED3, WED4, WED5, WED6, WED7,
  THU1, THU2, THU3, THU4, THU5, THU6, THU7,
  FRI1, FRI2, FRI3, FRI4, FRI5, FRI6, FRI7,
  EXP1, EXP2, ACT1, ACT2, ACT3,
};

typedef int crsid;

struct profess
{
  string name;
  bool full;
};

struct course
{
  classify      clsf; //not supported
  crsid         id;
  string        title;
  string        title_kr;
  int           credit;
  vector<crsid> requir;
};

struct section
{
  crsid   crsID;
  int     sectno; //0 means there's only
  string  profname;
  int     capacity;
  vector<lectime> lecture;
  int room; //not supported
};

struct profess_id_comp {
  bool operator() (const profess &a, const profess &b)
  { return a.name < b.name; }
};

struct course_id_comp {
  bool operator() (const course &a, const course &b)
  { return a.id < b.id; }
};

struct section_id_comp {
  bool operator() (const section &a, const section &b) {
    if(a.crsID == b.crsID)
      return a.sectno < b.sectno;
    else return a.crsID < b.crsID;
  }
};

crsid CrsidFromString(const string &str);

class Scheduler {
private:
  set<profess,profess_id_comp> whole_profess;
  set<course,course_id_comp> whole_courses;
  set<section,section_id_comp> whole_sections;

  bool load_1sik_openlects(const string &openlects);
  bool load_1sik_timetable(const string &timetable);

public:
  Scheduler() {}
  ~Scheduler() {}

  bool load_1sik(const string &openlects, const string &timetable);
};
