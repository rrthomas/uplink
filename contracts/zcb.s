global account contract_owner = 'COUNTER';
global account ms_zcb_issuer_issuer = 'ISSUER';
global asset zcb_asset_ = 'ASSET';
global int zcb_faceValue = FACEVALUE;
global int zcb_issuePrice = ISSUEPRICE;
global datetime zcb_maturity = "MATURITYDATE";
global datetime zcb_settlement = "SETTLEMENTDATE";
global datetime zcb_validityDate = "VALIDITYDATE";

transition initial -> terminal;

transition settlement_maturity -> terminal;

transition settlement_confirmation -> terminal;
transition settlement_confirmation -> settlement_maturity;
transition settlement_confirmation -> settlement_maturity;
transition initial -> settlement_confirmation;


@initial
zcb_confirmation() {
  if ((sender() == contract_owner)) {
    transitionTo(:settlement_confirmation);
  };
}

@settlement_confirmation
zcb_settlement_confirmation() {
  if ((sender() == contract_owner)) {
      ms_owner_joined_contract_owner = True;
      transferHoldings(contract_owner, zcb_asset_, zcb_issuePrice, ms_zcb_issuer_issuer);
      transitionTo(:settlement_maturity);
  };
}

@settlement_maturity
zcb_settlement_maturity() {
  if ((sender() == ms_zcb_issuer_issuer)) {
    after (zcb_maturity){
      transferHoldings(ms_zcb_issuer_issuer, zcb_asset_, zcb_faceValue, contract_owner);
      transitionTo(:terminal);
    };
  };
}
