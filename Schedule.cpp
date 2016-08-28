#include <cassert>
#include "Schedule.h"
#define FOR(n) for(int asdf=0; asdf<(n); asdf++)

int stoi(const string& str)
{
  int x = 0;

  for(int i=0; i<str.size(); i++)
  {
    if(str[i]<'0' || '9'<str[i]) break;
    x*=10; x+=str[i]-'0';
  } return x;
}

crsid CrsidFromString(const string &str)
{
  assert(str.size() == 6);

  string label(str.begin(), str.begin()+2);
  string code(str.begin()+2, str.end());

  int inf = stoi(code);
  int pre = 0;

  //we can use string-int-map for this
  if(label == "GS")
    pre = 1;
  else if(label == "EV")
    pre = 2;

  return pre*10000 + inf;
}

bool Scheduler::load_1sik_openlects(const string &openlects)
{
  ifstream file;

  //assume success
  file.open(openlects.c_str());

  //assume formatted
  int N;
  file >> N;

  for(int n=0; n<N; n++)
  {
    profess prf;
    course  crs;
    section sec;
    char    buff[128];

    file.ignore(100, '\t');
    file.ignore(100, '\t');
    file.get(buff, 7);//crs id
    crs.id = CrsidFromString(buff);
    file.ignore(100, '\t');
    file.ignore(100, '\t');
    assert(file.get() == '"');
    file.get(buff, 128, '\n');
    assert(file.get() == '\n');
    crs.title_kr = buff;      //crs title_kr
    file.get(buff, 128, '"');
    crs.title = buff;         //crs title
    file.ignore(100, '\t');
    file >> crs.credit;       //crs credit
    file.ignore(100, '\t');
    if(file.peek() != '\t') {
      file.get(buff, 128, '\t');
      prf.name = buff;        //prf name
    }
    file.ignore(100, '\t');
    file.get(buff, 128, '\t');
    prf.full = buff == string("전임");
    file.ignore(100, '\t');   //prf full
    if(file.peek() != '\t')
      file >> sec.sectno;
    else sec.sectno = 0;      //sec sectno
    file.ignore(100, '\t');
    file >> sec.capacity;
    file.ignore(100, '\t');
    if(file.peek() != '\n' && file.peek() != '\t') {
      while(1) {              //crs requir
        int no;
        file.get(buff, 7);
        no = CrsidFromString(buff);
        crs.requir.push_back(no);
        if(file.peek() == ' ') {
          //more requirment
          file.ignore(100, ' ');
          file.ignore(100, ' ');
        } else break;
      }
    } else crs.requir.clear();
    file.ignore(100, '\n');
    sec.crsID = crs.id;       //sec crsID
    sec.profname = prf.name;  //sec profname

    //line done; record them
    whole_profess.insert(prf);
    whole_courses.insert(crs);
    whole_sections.insert(sec);
  }

  file.close();

  //assume always success
  return true;
}

bool Scheduler::load_1sik_timetable(const string &timetable)
{
  return true;
}

bool Scheduler::load_1sik(const string &openlects,
                          const string &timetable)
{
  load_1sik_openlects(openlects);
  load_1sik_timetable(timetable);
}
