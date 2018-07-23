/*
This example data reconciliation workflow script ("smart contract") is for
EDUCATIONAL PURPOSES ONLY. It serves to demonstrates the syntax of the Uplink
scripting language. This script is licensed under an Apache 2.0 License, a
copy of which may be found at http://www.apache.org/licenses/LICENSE-2.0. By
using the script, you agree to the terms of this license.

Copyright 2018 Adjoint LLC
*/

/* ----------------------------- ZERO COST BOND --------------------------------

This example workflow models a situation where a bond issuer issues a zero cost
cost bond to an investor, usually at a steep discount compared to its face value
(the price which the issuer has to pay when buying back the bond).

------------------------------------------------------------------------------*/


global account bond_issuer; // The issuer of the bond
global account investor; // The investor who is buying the bond

global assetFrac2 the_asset; // USD/GBP/EUR etc.
global fixed2 face_value; // The face value
global fixed2 issue_price; // The issue price of the bond

global datetime offer_made; // The date at which the offer was made
global timedelta offer_validity; // How long the offer is valid
global datetime offer_expiry; // The date when the deployer can retract the offer

global datetime bond_issued; // The date at which the offer was accepted
global timedelta time_until_maturity; // The length of the period between issuing and maturity;
global datetime maturity; // The date of maturity


transition initial -> terminal;
transition initial -> offer_made;
transition offer_made -> initial;
transition offer_made -> contract_agreed;
transition contract_agreed -> terminal;

// Bond issuer may terminate the workflow if it is in the inital state
@initial {deployer()}
kill() {
  terminate("Workflow terminated from the initial state.");
}

// Instantiate an offer to a particular investor
@initial {deployer()}
make_offer( account investor_p,
            assetFrac2 asset_p,
            fixed2 face_value_p,
            fixed2 issue_price_p,
            timedelta offer_validity_p,
            timedelta time_until_maturity_p
          ) {
  bond_issuer = deployer();
  investor = investor_p;
  the_asset = asset_p;
  face_value = face_value_p;
  issue_price = issue_price_p;
  time_until_maturity = time_until_maturity_p;
  offer_made = now();
  offer_expiry = offer_made + offer_validity_p;
  transitionTo(:offer_made);
}

// Investor declines the offer
@offer_made {investor}
decline_offer() {
  transitionTo(:initial);
}

// Bond issuer may retract the offer after the offer validity has expired
@offer_made {bond_issuer}
retract_offer() {
  after(offer_expiry) {
    transitionTo(:initial);
  };
}

// Investor accepts the offer and the asset is transferred in the same step,
// so the investor can only accept the offer if he has sufficient liquidity.
@offer_made {investor}
accept_offer() {
  transferHoldings(investor, the_asset, issue_price, bond_issuer);
  bond_issued = now(); // The clock until maturity starts ticking.
  maturity = bond_issued + time_until_maturity;
  transitionTo(:contract_agreed);
}

// The bond issuer pays out the face value after maturity
@contract_agreed {bond_issuer}
settle() {
  after(maturity) {
    transferHoldings(bond_issuer, the_asset, face_value, investor);
    terminate("Bond issuer has paid out the face value to the investor.");
  };
}