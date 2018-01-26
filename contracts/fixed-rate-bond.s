global account issuer = 'ISSUER';
global account investor = 'INVESTOR';
global asset asset_ = 'ASSET';
global datetime startDate = "START";
global datetime maturityDate = "MATURITY";
global datetime validityDate = "VALIDITY";

global fixed6 faceValue = FACEVALUEf;
global fixed6 couponRate = 0.100000f;

global int numOfCouponPaymentsLeft = 12;
global int paymentCount = 0;

global fixed6 couponPayment;
global datetime paymentDate;

transition initial -> confirmation;
transition confirmation -> coupon_payments;
transition coupon_payments -> terminal;

transition coupon_payments -> return_fv;
transition return_fv -> novate_account;

// 1 year fixed rate bond 
// 10% coupon rate 
// monthly payment (12 terms)
// Actual/Actual day count basis 

@initial
initial() {
  after (startDate) {
    before (validityDate){
      if((sender() == investor)) {
        couponPayment = (faceValue * (couponRate / numberOfCouponPaymentsLeft));
        transitionTo(:confirmation);
      };
    };
  };
}

@confirmation 
confirmation() {
  before (validityDate){
    if((sender() == issuer)){
      transferHoldings(investor, asset_, faceValue, issuer);
      transitionTo(:coupon_payments);
    };
  };
}

@coupon_payments
coupon_payments() {

  // +1 day so last payment can be done on maturity date and principal repaid
  before (maturityDate + 1d) {
    if((sender() == issuer)) {
      
      // keep count of number of coupon payments left and that are to be made.
      paymentCount = paymentCount + 1;
      numOfCouponPaymentsLeft = numOfCouponPaymentsLeft - 1;

      // calculate payment date
      if((paymentCount == 1)){
        paymentDate = startDate + 1mo;
      }
      else{
        paymentDate = paymentDate + 1mo;
      };

      // make coupon payment to investor
      if((paymentDate <= maturityDate)){
        transferHoldings(issuer, asset_, couponPayment, investor);
      };
      
      // return principal to investor
      if((numOfCouponPaymentsLeft == 0)){
        transferHoldings(issuer, asset_, faceValue, investor);
        terminate("Fixed Rate Bond has reached maturity.");
      };

    };
  };
}

// account novation of investor
@@initial
return_fv () {
  before (maturityDate + 1) {
    novationInit(300);
    transferHoldings(issuer, asset_, faceValue, investor);
    transitionTo(:novate_account);
  }
}

@@novate_account 
novate_account (account newInvestor) {
  investor = newInvestor
  transferHoldings(newInvestor, asset_, faceValue, issuer);
  novationStop();
}