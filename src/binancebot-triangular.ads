with League.Strings;

with Binancebot.Parse;

package Binancebot.Triangular is


   type Arbitrage is range 1 .. 2;

   function Check_Buy_Buy_Sell
     (Poll : in out Binancebot.Parse.Type_TokInfo_Pull;
      Initial_Investment : in     Long_Float;
      Final_Price : out Long_Float) return Boolean;

   function Check_Buy_Sell_Sell
     (Poll : in out Binancebot.Parse.Type_TokInfo_Pull;
      Initial_Investment : in     Long_Float; Final_Price : out Long_Float) return Boolean;

   function Check_Profit_Loss
     (Total_price_after_sell : in Long_Float; Initial_investment : in Long_Float;
      Transaction_brokerage  : in Long_Float; Min_profit : in Long_Float) return Long_Float;

   procedure Perform_Triangular_Arbitrage
     (Poll : in out Binancebot.Parse.Type_TokInfo_Pull;
      Arbitrage_type        : in Arbitrage; Initial_investment : in Long_Float;
      Transaction_brokerage : in Long_Float; Min_profit : in Long_Float);

end Binancebot.Triangular;
