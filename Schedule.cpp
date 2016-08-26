#include "Schedule.h"

bool Scheduler::load_1sik(const string &openlects,
                          const string &timetable)
{
  ifstream file;

  //parse openlects--------------------------------

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
    file.ignore(2);
    file >> crs.id;           //crs id
    file.ignore(100, '\n');
    file.get(buff, 128, '"');
    crs.title = buff;         //crs title
    file.ignore(100, '\t');
    file >> crs.credit;       //crs credit
    file.ignore(100, '\t');
    if(file.peek() != '\t')
    {
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
    if(file.peek() != '\n' && file.peek() != '\t')
    {                         //crs requir
      while(1)
      {
        int no;
        file.ignore(2);
        file >> no;
        crs.requir.push_back(no);
        if(file.peek() == ' ')
        {
          //more requirment
          file.ignore(100, ' ');
          file.ignore(100, ' ');
        }
        else break;
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

  //parse timetable--------------------------------

  //assume success
  file.open(timetable.c_str());

  //assumes formatted




}

