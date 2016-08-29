#include <iostream>
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
  if(str.size() != 6)
    cout << '"' << str << '"' << str.size() << endl;

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

void parsing_tester1()
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

    //print out
    cout << "prf name   : " << prf.name << endl;
    cout << "prf full   : " << prf.full << endl;
    cout << "crs id     : " << crs.id << endl;
    cout << "crs titlek : " << crs.title_kr << endl;
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

void parsing_tester2()
{
  ifstream file;
  file.open("ex_timetable");

  set<crsid> tt[12][5];
  char buff[128];

  //assumes formatted
  int column[5];
  for(int i=0; i<5; i++) file >> column[i];
  int row[12];
  for(int j=0; j<12; j++) file >> row[j];
  int rmarg[11];
  for(int j=0; j<11; j++) file >> rmarg[j];

  file.ignore(100, '\n');

  //class(7), experiment(2), activity(3)
  for(int j=0; j<12; j++)
  {
    FOR(row[j])
    {
      vector<crsid> ROW[5];
      for(int i=0; i<5; i++) ROW[i].resize(column[i]);

      //first row, crsid
      for(int i=0; i<5; i++)
      {
        for(int x=0; x<column[i]; x++)
        {
          //cout << '(' << file.peek() << ')' << flush;
          if(file.peek() != '\n' && file.peek() != '\t')
          {
            crsid id;
            file.get(buff, 7);
            id = CrsidFromString(buff);
            ROW[i][x] = id;
            cout << id << ' ';
          }
          else cout << '|' << ' ';

          int drop = file.get();
          //double code
          while(drop == '/')
          {
            file.get(buff, 7);
            drop = file.get();
          }
          assert(drop == '\t' || drop == '\n' );
        }

        cout << '$' << endl;
      }

      //second row, profess
      //no need
      //file.getline(buff, 128); <- in case formatted better
      for(int i=0; i<5; i++)
      {
        for(int x=0; x<column[i]; x++)
        {
          //cout << '(' << file.peek() << ')';
          if(file.peek() != '\n' && file.peek() != '\t')
          {
            //if the cell is wraped with quot, since newline included
            if(file.peek() == '"')
            {
              assert(file.get() == '"');
              //file.ignore(100, '"');
              file.get(buff, 128, '"');
              cout << buff << ' ';
              assert(file.get() == '"');
            }
            else
            {
              if(i == 5-1 && x == column[5-1]-1)
                file.get(buff, 128, '\n');
              else file.get(buff, 128, '\t');
              cout << buff << ' ';
            }
          }
          else cout << '|' << ' ';

          int drop = file.get();
          assert(drop == '\t' || drop == '\n' );
        }

        cout << '$' << endl;
      }

      //third row, title - different from openlects
      //extract the sectoin number
      for(int i=0; i<5; i++)
      {
        for(int x=0; x<column[i]; x++)
        {
          //cout << '(' << file.peek() << ')' << endl;
          if(file.peek() != '\n' && file.peek() != '\t')
          {
            int sectno = 0;

            //if the cell is wraped with quot, since newline included
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

            cout << sectno << ' ';
          }
          else cout << '|' << ' ';

          //if(x!=column[i]) file.ignore(100, '\t');
          //else file.ignore(100, '\n');
          if(x == column[i]-1 && i == 5-1) file.ignore(100, '\n');
          else file.ignore(100, '\t');
        }

        cout << '$' << endl;
      }

      //fourth row, Room
      //not supported yet
      //file.getline(buff, 128); <- in case formatted better
      /*for(int i=0; i<5; i++) for(int x=0; x<column[i]; x++)
      {
        //if(file.peek() != '\t')
        //{
        //  //if the cell is wraped with quot, since newline included
        //  if(file.peek() == '"')
        //  {
        //    assert(file.get() == '"');
        //    file.ignore(100, '"');
        //    assert(file.get() == '"');
        //  }
        //}

        //if(x!=column[i]) file.ignore(100, '\t');
        //else file.ignore(100, '\n');

        if(x == column[i]-1 && i == 5-1) file.ignore(100, '\n');
        else file.ignore(100, '\t');
      }*/
      int drop = file.get();
      while(drop != '\n')
      {
        if(drop == '"')
          file.ignore(1000000, '"');
        drop = file.get();
      }
    }

    //drop margine
    if(j!=11)
    {
      FOR(rmarg[j])
        file.ignore(1000000, '\n');
    }
  }
}

int main()
{
  parsing_tester2();
  return 0;
}
