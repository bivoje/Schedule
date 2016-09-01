#include <limits>
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

struct celldat
{
  crsid id;
  int sectno;
  string profname;
  int room; //not supported yet
};

bool parse_cell(ifstream &file, celldat &cell, const int stat, const bool islast)
{
  char buff[128];

  //data row
  switch(stat)
  {
  case 1: {
  //first row, crsid
    if(file.peek() != '\n' && file.peek() != '\t')
    {
      file.get(buff, 7);
      cell.id = CrsidFromString(buff);
    }
    else cell.id = -1;

    //ignore double code
    int drop = file.get();
    while(drop == '/')
    {
      file.get(buff, 7);
      drop = file.get();
    }
    assert(drop == '\t' || drop == '\n' );
    break; }

  case 2: {
  //second row, profess
  //no need
    if(file.peek() != '\n' && file.peek() != '\t')
    {
      //if the cell is wraped with quot, for newline included
      if(file.peek() == '"')
      {
        assert(file.get() == '"');
        file.get(buff, 128, '"');
        assert(file.get() == '"');
      }
      else
      {
        if(islast) file.get(buff, 128, '\n');
        else file.get(buff, 128, '\t');
      }
      cell.profname = buff;
    }
    else cell.profname = "NONE";

    int drop = file.get();
    assert(drop == '\t' || drop == '\n' );
    break; }

  case 3: {
  //third row, title - different from openlects
  //extract the sectoin number
    if(file.peek() != '\n' && file.peek() != '\t')
    {
      int sectno = 0;

      //if the cell is wraped with quot, for newline included
      if(file.peek() == '"')
      {
        assert(file.get() == '"');
        while(file.peek() != '"')
        {
          if(file.get() == '(')
          {
            int next = file.peek();
            if(next<'1' || '9'<next)
              break;

            file >> sectno;
            break;
          }
        }
        file.ignore(100, '"');
      }
      else
      {
        while(file.peek() != '\t')
        {
          if(file.get() == '(')
          {
            int next = file.peek();
            if(next<'1' || '9'<next)
              break;

            file >> sectno;
            break;
          }
        }
      }

      cell.sectno = sectno;
    }
    else cell.sectno = -1;

    if(islast) file.ignore(100, '\n');
    else file.ignore(100, '\t');
    break; }

    default: assert(0);
  }

  //assume always success
  return true;
}

bool parse_row(ifstream &file, vector<celldat> cells[], const int stat, const int r, const int column[])
{
  //fourth row, room
  //not supported yet, ignore all
  //goto out of loop
  if(stat == 4)
  {
    int drop = file.get();
    while(drop != '\n')
    {
      if(drop == '"')
        file.ignore(numeric_limits<int>::max(), '"');
      drop = file.get();
    }

    //assume always success
    return true;
  }

  //day of a week (MON ~ FRI)
  for(int i=0; i<5; i++)
  {
    int Cn = column[i];
    //a cell for each day (& time interval)
    for(int x=0; x<Cn; x++)
      parse_cell(file, cells[i][x + r*Cn], stat,
                 i == 5-1 && x == Cn-1);
  }

  //assume always success
  return true;
}

bool parse_ROW(ifstream &file, vector<celldat> cells[], const int r, const int column[])
{
  //data row (1 ~ 4)
  for(int dr = 1; dr<=4; dr++)
  {
    parse_row(file, cells, dr, r, column);
  }

  //assume always success
  return true;
}

bool parse_interval(ifstream &file, const int Rn, const int marg, const int column[])
{
  //data representing ROW (1 or 2)
  vector<celldat> cells[5];

  for(int i=0; i<5; i++)
    cells[i].resize(column[i]*Rn);

  for(int r=0; r<Rn; r++)
    parse_ROW(file, cells, r, column);

  //drop margine
  for(int a=0; a<marg; a++)
    file.ignore(numeric_limits<int>::max(), '\n');

  //assume always success
  return true;
}

bool Scheduler::load_1sik_timetable(const string &timetable)
{
  ifstream file;

  //assume success
  file.open(timetable.c_str());

  //assume formatted
  int column[5];
  for(int i=0; i<5; i++) file >> column[i];
  file.ignore(100, '\n');
  int row[12];
  for(int j=0; j<12; j++) file >> row[j];
  file.ignore(100, '\n');
  int rmarg[12];
  for(int j=0; j<11; j++) file >> rmarg[j];
  rmarg[11] = 0;
  file.ignore(100, '\n');

  //12 time interval, class(7), experiment(2), activity(3)
  for(int j=0; j<12; j++)
  {
    parse_interval(file, row[j], rmarg[j], column);
  }

  //assume always success
  return true;
}

bool Scheduler::load_1sik(const string &openlects,
                          const string &timetable)
{
  load_1sik_openlects(openlects);
  load_1sik_timetable(timetable);
}
