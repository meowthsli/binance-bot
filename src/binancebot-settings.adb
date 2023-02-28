with Config;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Binancebot.Settings is


   procedure Get_Settings_From_File(Settings : out Type_Settings;
                                   Fileconf : String := "config.ini") is
      use Config;

      c: Configuration;
   begin
      Init(c, Fileconf);
      -- Also, Check IS_SET !!!
      declare
         API_Token : String := Value_Of(c, "API Keys", "API_Key");
         Private_Key : String := Value_Of(c, "API Keys", "Private_Key");
         INVESTMENT_AMOUNT_DOLLARS : Long_Float := Value_Of(c, "Trading", "INVESTMENT_AMOUNT_DOLLARS");
         MIN_PROFIT_DOLLARS : Long_Float := Value_Of(c, "Trading", "MIN_PROFIT_DOLLARS");
         BROKERAGE_PER_TRANSACTION_PERCENT : Long_Float := Value_Of(c, "Trading", "BROKERAGE_PER_TRANSACTION_PERCENT");
      begin
         if API_Token'Length = 64 then
            Settings.API_Token := API_Token;
         else
            Ada.Integer_Text_IO.Put(API_Token'Length);
         end if;

         if Private_Key'Length = 64 then
            Settings.Private_Key := Private_Key;
         else
            Ada.Integer_Text_IO.Put(API_Token'Length);
         end if;

         Settings.INVESTMENT_AMOUNT_DOLLARS := INVESTMENT_AMOUNT_DOLLARS;
         Settings.MIN_PROFIT_DOLLARS := MIN_PROFIT_DOLLARS;
         Settings.BROKERAGE_PER_TRANSACTION_PERCENT := BROKERAGE_PER_TRANSACTION_PERCENT;
      end;
   end Get_Settings_From_File;

end Binancebot.Settings;
