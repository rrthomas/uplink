
global msg customer_reference;
global msg insured_obj_reference;
global account insuree;

global msg tariff_reference = 'GUID_RATING_ENGINE';
global msg terms_conditions = 'LINK_TO_TERMS_AND_CONDITIONS';
global msg internal_contract_identifier = '10001';
global msg product_identifier = 'MTPL';

global datetime start_date = "2018-02-01T12:00:00+00:00";
global datetime end_date = "2018-02-28T12:00:00+00:00";

global fixed2 day_premium = 1.00f;
global fixed2 premium_earned = 0.00f;
global fixed2 premium_written = 0.00f;

global fixed2 premium_reserve = 28.00f;
global fixed2 coverage_term = 28.00f;

global account insurer = 'INSURER';
global asset asset_ = 'ASSET';

// main graph
transition initial -> confirmation;
transition confirmation -> in_force;
transition in_force -> terminal;

// novation sub-graphs 
transition in_force -> update_premiums;
transition in_force -> cancel_policy;
transition in_force -> change_enddate; 

// insurer set customer details
@initial
initial(account insure_this_person, msg customer_ref, msg insured_obj){
  before(start_date) {
    if((sender() == insurer)) {
      insuree = insure_this_person;
      customer_reference = customer_ref;
      insured_obj_reference = insured_obj;
      transitionTo(:confirmation);
    };
  };
}


// customer confirms coverage
@confirmation
confirmation() {
  before(start_date) {
    if((sender() == insuree)) { 
      premium_written = coverage_term * day_premium;
      premium_reserve = premium_written - premium_earned;
      transitionTo(:in_force);
    };
  };
}


// insurance contract is now in force
// method should be called once a day
@in_force
in_force(){
  after(start_date){
    if((sender() == insurer)){
      premium_earned = premium_earned + day_premium;
      premium_written = coverage_term * day_premium;
      premium_reserve = premium_written - premium_earned;
      transferHoldigs(insuree, asset_, day_premium, insurer);

    };
  };
  after(end_date){
    terminate("coverage has ended");
  };
}


@@update_premium
update_premium (fixed2 new_premium) {
  if((sender() == insurer)) {
    novationInit(300);
    day_premium = new_premium;
    novationStop();
  };
}

@@change_enddate
change_enddate (datetime new_enddate, fixed2 new_term){
  if((sender() == insurer )) {
    novationInit(300);
    coverage_term = new_term;
    end_date = new_enddate;
    novationStop();
  };
}

@@cancel_policy
cancel_policy(){
  if((sender() == insurer)) {
    novationInit(300);
    terminate("policy is canceled");
    novationStop();
  };
}