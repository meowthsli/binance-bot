with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Wide_Wide_Text_IO;

with League.Strings;

with Binancebot.Util;
with Binancebot.Net;
with Binancebot.Orders;

package body Binancebot.Triangular is

   function Check_Buy_Buy_Sell (Poll : in out Binancebot.Parse.Type_TokInfo_Pull;
                                Initial_Investment : in Long_Float;
                                Final_Price : out Long_Float
                               ) return Boolean is
      Investment_Amount1 : Long_Float := Initial_Investment;
      TCurrent_Price1 : League.Strings.Universal_String;
      Current_Price1 : Long_Float;
      Buy_Quantity1 : Long_Float;
      Buy_Quantity2 : Long_Float;
      Investment_Amount2 : Long_Float;
      Current_Price2 : Long_Float;
      TCurrent_Price2 : League.Strings.Universal_String;
      Investment_Amount3 : Long_Float;
      Current_Price3 : Long_Float;
      TCurrent_Price3 : League.Strings.Universal_String;
      Sell_Quantity3 : Long_Float;

      function Check_If_Float_Zero(Price : Long_Float) return Boolean is
      begin
         return Binancebot.Util.Isclose(Price,0.0,0.001);
      end Check_If_Float_Zero;

   begin

      Binancebot.Parse.Apply_TickerPrices_To_Poll(Poll => Poll,
                                                  Prices => Binancebot.Net.Get_Ticker_Price(Poll));

      Current_Price1 := Poll.First_Price;
      Current_Price2 := Poll.Second_Price;
      Current_Price3 := Poll.Third_Price;


      return false when Check_If_Float_Zero(Current_Price1) or Current_Price1 = 0.0;

      Buy_Quantity1 := Investment_Amount1 / Current_price1;--,8);

      investment_amount2 := buy_quantity1;

      return false when Check_If_Float_Zero(Current_Price2) or Current_Price2 = 0.0;

      Buy_Quantity2 := Investment_Amount2 / Current_price2;--,8);

      Investment_Amount3 := buy_quantity2;

      return false when Check_If_Float_Zero(Current_Price3) or Current_Price3 = 0.0;

      Sell_Quantity3 := buy_quantity2;

      Final_Price := Sell_Quantity3 * Current_Price3;--,3)


      return true;
   end Check_Buy_Buy_Sell;


   function Check_Buy_Sell_Sell (Poll : in out Binancebot.Parse.Type_TokInfo_Pull;
                                Initial_Investment : in Long_Float;
                                Final_Price : out Long_Float
                                ) return Boolean is

      Investment_Amount1 : Long_Float := Initial_Investment;
      TCurrent_Price1 : League.Strings.Universal_String;
      Current_Price1 : Long_Float;
      Buy_Quantity1 : Long_Float;
      Buy_Quantity2 : Long_Float;
      Investment_Amount2 : Long_Float;
      Current_Price2 : Long_Float;
      TCurrent_Price2 : League.Strings.Universal_String;
      Investment_Amount3 : Long_Float;
      Current_Price3 : Long_Float;
      TCurrent_Price3 : League.Strings.Universal_String;
      Sell_Quantity2 : Long_Float;
      Sell_Quantity3 : Long_Float;
      Sell_Price2: Long_Float;

      function Check_If_Float_Zero(Price : Long_Float) return Boolean is
      begin
         return Binancebot.Util.Isclose(Price,0.0,0.001);
      end Check_If_Float_Zero;


      use Binancebot.Util;
   begin

      Binancebot.Parse.Apply_TickerPrices_To_Poll(Poll => Poll,
                                                  Prices => Binancebot.Net.Get_Ticker_Price(Poll));

      Current_Price1 := Poll.Third_Price;
      Current_Price2 := Poll.Second_Price;
      Current_Price3 := Poll.First_Price;

      Final_Price := 0.0;
      if not Check_If_Float_Zero(current_price1) or Current_Price1 /= 0.0 then
         Buy_Quantity1 := Investment_amount1 / current_price1;--, 8)

         Investment_Amount2 := Buy_quantity1;
         if Check_If_Float_Zero(Current_Price2) or Current_Price2 /= 0.0 then
            Sell_Quantity2 := Buy_quantity1;
            Sell_Price2 := Sell_Quantity2 * Current_Price2;--,8)

            Investment_Amount3 := Sell_Price2;
            if Check_If_Float_Zero(Current_Price3) or Current_Price3 /= 0.0 then
               Sell_quantity3 := sell_price2;
               Final_Price := Sell_quantity3 * Current_price3;--,3)

               return true;
            end if;
         end if;
      end if;

      return false;

   end Check_Buy_Sell_Sell;


   function Check_Profit_Loss(Total_price_after_sell : in Long_Float;
                              Initial_investment : in Long_Float;
                              Transaction_brokerage : in Long_Float;
                              Min_profit : in Long_Float) return Long_Float is
      Apprx_brokerage : Long_Float :=  (Initial_investment / 100.0) * Transaction_brokerage;
      Min_profitable_price : Long_Float := Initial_Investment + Apprx_brokerage + Min_profit;
      Profit_loss : Long_Float := Total_price_after_sell - Min_profitable_price;--,3)
   begin
      return profit_loss;
   end Check_Profit_Loss;

   procedure Perform_Triangular_Arbitrage(Poll : in out Binancebot.Parse.Type_TokInfo_Pull;
                                          Arbitrage_type : in Arbitrage;
                                          Initial_investment : in Long_Float;
                                          Transaction_brokerage : in Long_Float;
                                          Min_profit : in Long_Float ) is
      Final_price : Long_Float;
      Profit_loss : Long_Float;
      Result : Boolean;

   begin

      if Arbitrage_type = 1 then
        -- Check this combination for triangular arbitrage: scrip1 - BUY, scrip2 - BUY, scrip3 - SELL
         Result := check_buy_buy_sell(Poll,Initial_investment,Final_price);

      elsif Arbitrage_type = 2 then
         -- Check this combination for triangular arbitrage: scrip1 - BUY, scrip2 - SELL, scrip3 - SELL
         Result := check_buy_sell_sell(Poll,Initial_investment, Final_price);
      end if;

      if Result /= False then

         Profit_loss := Check_profit_loss(final_price,initial_investment, transaction_brokerage, min_profit);

        if (Profit_loss > 0.0) then
            declare
               Tmp0: Long_Float := Final_price - Initial_investment;
            begin
               -- UNCOMMENT THIS LINES TO PLACE THE ORDERS
               Binancebot.Orders.Place_trade_orders(arbitrage_type, Poll, initial_investment);
               if Arbitrage_type = 1 then
                  Ada.Text_IO.Put(Arbitrage_Type'Image & " " & Util.U2S(Poll.First_Pair) & "/" & Util.U2S(Poll.Second_Pair) & "/" & Util.U2S(Poll.Third_Pair) & " Profit/Loss: ");
               elsif Arbitrage_type = 2 then
                  Ada.Text_IO.Put(Arbitrage_Type'Image & " " & Util.U2S(Poll.Third_Pair) & "/" & Util.U2S(Poll.Second_Pair) & "/" & Util.U2S(Poll.First_Pair) & " Profit/Loss: ");
               end if;
               Ada.Long_Float_Text_IO.Put(Tmp0, Exp => 0);
               Ada.Text_IO.Put(" Real Profit: ");
               Ada.Long_Float_Text_IO.Put(Profit_loss, Aft => 10, Exp => 0);
               Ada.Text_IO.New_Line;
            end;
         end if;
      end if;
   end Perform_Triangular_Arbitrage;
end Binancebot.Triangular;
