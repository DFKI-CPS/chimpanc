#include <systemc.h>
#include <unordered_set>
#include <array>
#include <string>

namespace esl {

typedef int Building;
typedef int Person;

#define NUM_DOORS 6
#define NUM_PERSONS 4
#define NUM_BUILDINGS 4

#define NO_PERSON (-1)
#define DEFAULT_BUILDING 0

#define TIMEOUT_RED 5
#define TIMEOUT_GREEN 20
#define TIME_UNIT SC_SEC

/* Default number of steps in the simulation */
#define NUM_STEPS 20

/* The authentication datatbase */
class Users {
private:
  static std::unordered_set<Building> aut[NUM_PERSONS];
  static Building sit[NUM_PERSONS];
  
public:
  Users()
  {
    for (Person p= 0; p< NUM_PERSONS; p++)
      {
  aut[p]= std::unordered_set<Building>();
  sit[p]= DEFAULT_BUILDING;
      }
  }
  
  static bool admitted(Person p, Building b)
  {
    return (aut[p].count(b) > 0);
  }

  static void set_sit(Person p, Building b)
  {
    sit[p]= b;
  }

  static Building get_sit(Person b)
  {
    return sit[b];
  }
  
  static void add_aut(Person p, Building b)
  {
    aut[p].insert(b);
  }
  
};

std::unordered_set<Building> Users::aut[NUM_PERSONS];
Building Users::sit[NUM_PERSONS];

std::string log_time()
{
  return "["+ sc_time_stamp().to_string()+ "] ";
}

/* An LED, which can be turned on and off. */
SC_MODULE(LED) {
  sc_in<bool> in;

  SC_CTOR(LED)
  {
    SC_METHOD(blink);
    sensitive << in;
  }

  void blink()
  {
    if (in.read()) 
      cout << log_time() << "LED "<< name() << ": ON\n";
    else
      cout << log_time() << "LED "<< name() << ": OFF\n";
  }
};




/* The gate, implementing the class Door */
SC_MODULE(Gate)
{
  
  sc_in<Person> card;
  // Turnstile interface:
  sc_in<bool>  passed;
  sc_out<bool> unlock;
  // Two lights for green and red:
  sc_out<bool> green;
  sc_out<bool> red;
  
public:
  // Better: make it private, include values as constructor args.
  Building org;
  Building dest;
  
 private:
  Person dap= NO_PERSON; // Person currently passing

 public:
  SC_CTOR(Gate)
  {
    dap= NO_PERSON;    
    SC_THREAD(operate);
  }

  void accept()
  {
    cout << log_time() << "Person " << dap << " accepted.\n";
    green.write(true);
    unlock.write(true);
  }

  void pass_thru()
  {
    cout << log_time() << "Person " << dap << " has gone to " << dest << "\n";
    Users::set_sit(dap, dest);
    green.write(false);
    unlock.write(false);
    dap= NO_PERSON;
  }

  void off_grn()
  {
    green.write(false);
    unlock.write(false);
    dap= NO_PERSON;
  }

  void refuse()
  {
    cout << log_time() << "Person " << dap << " refused.\n";
    red.write(true);
    dap= NO_PERSON;
  }
  
  void off_red()
  {
    red.write(false);
    dap= NO_PERSON;
  }
    
  void operate()
  {
    while (true) {
      wait(card.value_changed_event());
      dap= card.read();
      if (Users::admitted(dap, dest))
  {
    accept();
    wait(sc_time(TIMEOUT_GREEN, TIME_UNIT),
                 passed.posedge_event());
    if (passed.read())
      pass_thru(); 
    else
      off_grn();
  }
      else
  {
    refuse();
    wait(TIMEOUT_RED, TIME_UNIT);
    off_red();
  }
    }
  }
};


bool pick_bool()
{
  return rand() % 2 == 0;
}


/* The turnstile has an input to be unlocked, and an output
 * indicating wether someone has passed. */
SC_MODULE(turnstile) {
  sc_in<bool> unlock;
  sc_out<bool> passed;

  SC_CTOR(turnstile)
  {
    SC_THREAD(operate);
    // sensitive << unlock;
  }
 
  void operate()
  {
    int t;
    bool b;
    
    while (true)
      {
  wait (unlock.posedge_event());
  cout << log_time() << "Turnstile " << name() << " unlocked.\n";
  /* Wait random amount of time... */
  wait(rand() % (TIMEOUT_GREEN/2)+ TIMEOUT_GREEN/2, TIME_UNIT);
  b= pick_bool();
  cout << log_time() << "Turnstile " << name () << (b ? ": somebody " : ": nobody ") << "passed.\n";
  passed.write(b);
      }
  }
};    

/* A complete door consists of a gate (controller), two LEDs, and a turnstile */
SC_MODULE(Door)
{
  sc_out<Person> card;

private:
  sc_signal<bool> green_sig;
  sc_signal<bool> red_sig;
  sc_signal<bool> unlock_sig;
  sc_signal<bool> pass_sig;
  sc_signal<Person> card_sig;
  
  LED grn;
  LED red;
  turnstile ts;
  Gate gc;

public:
  SC_CTOR(Door) : grn("Green"), red("Red"), ts("TS"), gc("GC")
  {    
    gc.red(red_sig);
    red.in(red_sig);

    gc.green(green_sig);
    grn.in(green_sig);

    gc.card(card_sig);
    card(card_sig);
    
    gc.unlock(unlock_sig);
    ts.unlock(unlock_sig);
    
    gc.passed(pass_sig);
    ts.passed(pass_sig);

    // SC_METHOD(operate);
    // sensitive >> card;
  }

  void setOrgDest(int org, int dest)
  {
    gc.org= org;
    gc.dest= dest;
  }
  
  void operate()
  {
    approach(card.read());
  }

  void approach(Person p)
  {
    cout << log_time() << "Person "<< p << " approaching door " << name() << "\n";
    card.write(p);
  }
  
};

SC_MODULE(testGen)
{
  Door *doors[NUM_DOORS]; 
  int num_steps;
  
  SC_CTOR(testGen)
  {
    doors[0] = new Door("DoorA");
    doors[1] = new Door("DoorB");
    doors[2] = new Door("DoorC");
    doors[3] = new Door("DoorD");
    doors[4] = new Door("DoorE");
    doors[5] = new Door("DoorF");
    
    /* Connect doors */
    doors[0]->setOrgDest(1, 2);
    doors[1]->setOrgDest(2, 1);
    doors[2]->setOrgDest(2, 3);
    doors[3]->setOrgDest(3, 4);
    doors[4]->setOrgDest(4, 3);
    doors[5]->setOrgDest(4, 1);

    SC_THREAD(driver);
  }

  void driver()
  {
    Person p;
    Building b;
    int c;

    for (int i= 0; i< num_steps; i++)
      {
  cout << log_time() << "#### New simulation step ################\n";
  p= rand() % NUM_PERSONS;
  c= rand() % NUM_DOORS;
  // cout << "Person "<< p<< " approaching door "<< c << "\n";
  doors[c]->approach(p);
  wait(20, SC_SEC);
  cout << log_time() << "Person "<< p<< " now in building "<< Users::get_sit(p) << "\n";
      }
  }
};



int sc_main(int argc, char* argv[])
{
  int steps= NUM_STEPS;
  
  Building permissions[NUM_PERSONS][NUM_BUILDINGS]=
    { {1, 2, 3, 4},
      {1, 2},
      {2, 3},
      {1, 2, 4},
    };

  if (argc > 1)
    {
      std::stringstream s;      
      s << argv[1];
      s >> steps;
    }

  /* Initialize controller */
  for (Person p= 0; p< NUM_PERSONS; p++)
    for (Building b= 0; b< NUM_BUILDINGS; b++)
      {
  if (permissions[p][b])
    Users::add_aut(p, permissions[p][b]);
      }
  
  testGen tg("TestGen");
  tg.num_steps= steps;
  
  /* Run the test */
  cout << "Starting.\n";
  sc_start();
  
  return 0;
}  

}