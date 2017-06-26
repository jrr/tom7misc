
struct trivialc : public channel {
  trivialc(string & n)  { name = n; }

  /*
    virtual void kick(user * kicker, user * kickme, string msg);
    
    virtual void sendf(const char *, ...);
    virtual void nicksendf(const char *, const char *, ...);
  */
  virtual void speak(user * u, string style, string msg);
  /*
    virtual void sendexceptf (user * except, const char * msg, ...);
    
    virtual void settopic(user *, string);
    
    virtual list<presence> * inchannel(user * u);
    
    virtual void changenick(user *, string);
    
    virtual void modechange(user *, string);
    virtual void sendmode(user *);
  */    
  virtual int join (user *,string);
  /*
    virtual void part (user *);
    virtual void quit (user *,string);
    virtual void sendnames (user *);
    virtual void sendtopic (user *);
    
    virtual void expunge(list<presence> *);
  */
  //  virtual ~channel();

};
