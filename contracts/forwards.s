global account seller = 'SELLER';
global account buyer = 'BUYER';
global asset cashAsset = 'CASHASSET';
global asset underlying = 'UNDERLYING';
global fixed6 strike = STRIKE;
global fixed6 multiplier = MULTIPLIER;
global datetime maturityDate = "MATURITY";
global datetime validityDate = "VALIDITY";

transition initial -> confirmation;
transition confirmation -> settlement_seller;
transition settlement_seller -> settlement_buyer;
transition settlement_buyer -> terminal;

@initial 
initial(){
  before (validityDate){
    if ((sender() == seller)) {
      transitionTo(:confirmation);
    }; 
  };
}


@confirmation
confirmation(){
  before (validityDate){
    if ((sender() == buyer)) {
      transitionTo(:settlement_seller);
    }; 
  };
}

@settlement_seller(){
  settlement_seller(){
    after (maturityDate){
      if ((sender() == seller)) {
        transferHoldings(buyer, cashAsset, strike * multiplier, seller);
        transitionTo(:settlement_buyer);
      };
    };
  }
}

@settlement_buyer(){
  settlement_buyer(){
    after (maturityDate) {
      if ((sender() == buyer)) {
        transferHoldings(seller, underlying, multiplier, buyer);
        transitionTo(:terminal);
      };
    };
  }
}

