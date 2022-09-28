--  Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
--  This file is free software: you may copy, redistribute and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 2 of the License, or (at your
--  option) any later version.
--
--  This file is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Strings.Unbounded;
with AdaBase;                      use AdaBase;
with AdaBase.Statement.Base;       use AdaBase.Statement.Base;
with AdaBase.Driver.Base.MySQL;    use AdaBase.Driver.Base.MySQL;
with AdaBase.Statement.Base.MySQL; use AdaBase.Statement.Base.MySQL;
with Interfaces.C;                 use Interfaces.C;
with Record_Types;                 use Record_Types;

package db_routines is

   subtype Database_Driver  is AdaBase.Driver.Base.MySQL.MySQL_Driver;
   subtype Stmt_Type_Local  is AdaBase.Statement.Base.MySQL.MySQL_statement;
   subtype Stmt_Type_access is AdaBase.Statement.Base.MySQL.MySQL_statement_access;
   DR : AdaBase.Driver.Base.MySQL.MySQL_Driver;

   procedure DB_Connect;
   procedure DB_Disconnect;
   function  Get_Bill_Date (t : tmrec) return bill_date_details;
   procedure Bill_Date (t :  in out tmrec);
   function  Get_Earliest_Start return long;
   function  Get_Last_TS return Interfaces.C.long;
   function  Get_Last_TS_Run return Interfaces.C.long;
   function  Get_Power_Usage (p : periodrec; pt : graph_period_type; ctime : tmrec) return powerrec;
   procedure Set_Account_Details (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306);

private
   DB_Host  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   Database : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_User  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_Pass  : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
   DB_Port  : Integer := 3306;
end db_routines;
