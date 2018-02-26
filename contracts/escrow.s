// NOTE: All accounts and asset addresses assigned depict random, but valid
//       addresses. These will be overwritten in "sellerSetup" and "buyerSetup".
// sell side
global account sellerSeller  = '6pxGdGG6nQP3VoCW7HoGkCGDNCiCEWP3P5jHtrvgphBc';
global account sellerBuyer   = 'EUSS9VgTfyv1423JB6zEHF5meyNNGthrsvP3PUUrsKdu';
global assetDisc sellerAssetSell = '39oS7aToiKTazHDL7hu5ktbBUETRzVFybwGbhwj2DifC';
global assetDisc sellerAssetBuy  = '37K4VN7vRZ9hUm8tdMj2DQVgPhDixKQsZfNonsCnCgk1';
global int sellerAmountSell  = 0;
global int sellerAmountBuy   = 0;

// buy side
global account buyerSeller  = 'EPkP9xCnPSsM7ffK1oq2GLjFYikJMXKAbtCNKFmi7Ck6';
global account buyerBuyer   = '5EASMmbppLAbRr2RNfHHzek4wT5vuEP7jAhmdh8amzhn';
global assetDisc buyerAssetSell = '4sxds4MCJWhTRDbpxb7UP8iAVA27gHuU7t8ckfsyELzp';
global assetDisc buyerAssetBuy  = 'rgdUJFYM5JLMCAGKpHGFACRntefoS1pgJohvaM2u8Zo';
global int buyerAmountSell  = 0;
global int buyerAmountBuy   = 0;


global bool sellerSetupDone     = False;
global bool buyerSetupDone      = False;
global bool sellerTransferDone  = False;
global bool buyerTransferDone   = False;
global bool payOutDone          = False;

transition initial -> sellerSetup;

@initial
init() {
  transitionTo(:sellerSetup);
}

@sellerSetup
sellerSetup(account buyer, assetDisc assetSell, assetDisc assetBuy,
            int amountSell, int amountBuy) {
  if (!sellerSetupDone) {
    sellerSeller     = sender();
    sellerBuyer      = buyer;
    sellerAssetSell  = assetSell;
    sellerAssetBuy   = assetBuy;
    sellerAmountSell = amountSell;
    sellerAmountBuy  = amountBuy;

    sellerSetupDone  = True;
  };
}

@buyerSetup
buyerSetup(account seller, assetDisc assetSell, assetDisc assetBuy,
           int amountSell, int amountBuy) {
  if (!buyerSetupDone) {
    buyerSeller     = seller;
    buyerBuyer      = sender();
    buyerAssetSell  = assetSell;
    buyerAssetBuy   = assetBuy;
    buyerAmountSell = amountSell;
    buyerAmountBuy  = amountBuy;

    buyerSetupDone = True;
  };
}

@transferSell
transferSell() {
  if (sender() == sellerSeller) {
      if (sellerSetupDone && buyerSetupDone && (!sellerTransferDone)) {
        transferTo(sellerAssetSell, sellerAmountSell);
        sellerTransferDone = True;
    };
  };
}

@transferBuy
transferBuy() {
  if (sender() == buyerBuyer) {
      if (sellerSetupDone && buyerSetupDone && (!buyerTransferDone)) {
        transferTo(buyerAssetBuy, buyerAmountBuy);
        buyerTransferDone = True;
    };
  };
}

@payOut
payOut() { // can be called by buyer or seller
  if (buyerTransferDone && sellerTransferDone && (!payOutDone)) {
    if ((sender() == sellerSeller) || (sender() == buyerBuyer)) {
      if ((sellerSeller == buyerSeller) &&
          (sellerBuyer  == buyerBuyer)  &&
          (sellerAssetSell == buyerAssetSell) &&
          (sellerAssetBuy  == buyerAssetBuy)  &&
          (sellerAmountSell == buyerAmountSell) &&
          (sellerAmountBuy  == buyerAmountBuy)) {

          // transfer from contract to counterparties
          transferFrom(sellerAssetSell, sellerAmountSell, sellerBuyer);
          transferFrom(buyerAssetBuy, buyerAmountBuy, buyerSeller);

          payOutDone = True;
      } else { };
    } else { };
  } else { };
}
