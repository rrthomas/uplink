
global msg product_description = 'https://www.ukspassociation.co.uk/product/1156/';
global msg product_type = 'https://www.ukspassociation.co.uk/product-codes/protected-growth/protected-digital-1150/';

global account issuer = 'ISSUER';
global asset asset_ = 'ASSET'; // should be GBP
global fixed6 deposit = 'DEPOSIT';

global fixed6 return_calc = 0.200000f;
global fixed6 threshold_calc = 0.950000f;

global datetime closingDate = "2018-02-02T12:00:00+00:00";
global datetime isaTransferDate = "2018-12-01T16:30:00+00:00";
global datetime strikeDate = "2018-12-02T12:00:00+00:00";

global datetime startFinalDate = "2022-15-08T16:30+00+00";
global datetime maturityDate "2023-13-02T16:30:00+00:00"; 

global fixed6 counter = 0.000000f;
global fixed6 closing_level_sum = 0.000000f;
global fixed6 FTSE100_initial = 0.000000f; 
global fixed6 FTSE100_final = 0.000000f;
global fixed6 payout = 0.000000f;

transition initial -> confirmation;
transition confirmation -> determine_final_level;
transition determine_final_level -> settlement;
settlement -> terminal;

// investor sends deposit amount 
// method checks if its before the closing date and if  deposit is >= 3000 (assuming pounds)
// transfers deposit from investor to issuer
@initial
initial(fixed6 new_deposit) {
  before (closingDate){
    if((sender() == investor) && (new_deposit >= 3000)) {
      deposit = new_deposit; 
      transferHoldings(investor, asset_, deposit, issuer);
      transitionTo(:confirmation);
    };
  };
}


// issuer sends closing level price at 16:30 to 16:35 on Feb. 12 2018
// contract saves initial index level
@confirmation 
confirmation(fixed6 closing_price_FTSE100) {
  after (strikeDate){
    if((sender() == issuer)){
      FTSE100_initial = close_price_FTSE100;
      transitionTo(:determine_payout);
    };    
  };
}

// method should be called with data feed every day between Aug 15 2022 and 
// Feb 13 2023 at 16:30 both days inclusive
@determine_final_level
determine_final_level(fixed6 closing_price){
  after(startFinalDate) {
    before(maturityDate) {
      if((sender() == issuer)){
        if((isBusinessDayUK(now()))){
          closing_level_sum = closing_level_sum + closing_price;
          counter = counter + 1.000000f;
        };
      }; 
    };
    after(maturityDate){
      if((sender() == issuer)){
        FTSE100_final = closing_level_sum / counter
        transitionTo(:settlement);
      };
    };
  };
}

@settlement
settlement(){
  if((sender() == issuer)){
    
    // final index price is greater than 95% of initial index price
    // then return deposit and 20% profit
    if((FTSE100_final > (FTSE100_initial * threshold_calc))) {
      payout = (deposit + (deposit * return_calc));
      transferHoldings(issuer, asset_, payout, investor);
      terminate("returning deposit + 20%");
    };

    // final index price is lower or equal to 95% of initial index price
    // then return only the deposit
    if((FTSE100_final =< (FTSE100_initial * threshold_calc))) {
      transferHoldings(issuer, asset_, deposit, investor);
      terminate("returning deposit");
    };

  };
}