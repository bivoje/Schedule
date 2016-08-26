#include <iostream>
#include "Schedule.h"

void parsing_tester()
{
  ifstream file;
  file.open("ex_openlects");

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
    if(file.peek() != '\n' && file.peek() != '\t')   //crs requir
    {
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
    sec.crsID = crs.id;
    sec.profname = prf.name;

    //print out
    cout << "prf name   : " << prf.name << endl;
    cout << "prf full   : " << prf.full << endl;
    cout << "crs id     : " << crs.id << endl;
    cout << "crs title  : " << crs.title << endl;
    cout << "crs credit : " << crs.credit << endl;
    if(crs.requir.size() == 0)
      cout << "crs requir : none" << endl;
    else for(int i=0; i<crs.requir.size(); i++)
      cout << "crs requir : " << crs.requir[i] << endl;
    cout << "sec crsid  : " << sec.crsID << endl;
    cout << "sec sect#  : " << sec.sectno << endl;
    cout << "sec prfnam : " << sec.profname << endl;
    cout << "sec cap    : " << sec.capacity << endl;

    cout << "===============================================" << endl;
    cout << endl;
  }
}
