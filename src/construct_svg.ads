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

with Record_Types; use Record_Types;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Construct_SVG is

   procedure Write_SVG          (power_details, power_details_off : pow_array);
   procedure Write_SVG          (power_details                    : pow_array; rtype : graph_period_type);

private
   procedure Add_Line           (X1, X2 : Integer; Y1 : Float);
   procedure Add_Line_Dashed    (X1, X2 : Integer; Y1 : Float);
   procedure Add_Min_Max_Tot    (MinMax   : Min_Max; Total : Long_Integer);
   procedure Add_Min_Max_Tot    (MinMax   : Min_Max; MinMax_Offset : Min_Max; Total : Long_Integer; Total_Offset : Long_Integer);
   procedure Add_Preamble;
   procedure Add_Postamble;
   procedure Add_Title          (Title    : String);
   procedure Add_Tooltip_Area;
   procedure Add_Usage          (power_details : pow_array; uo : use_offset);
   procedure Add_Usage_Day      (power_details : pow_array);
   procedure Add_X_Axis_Day     (power_details : pow_array);
   procedure Add_X_Axis_Month   (power_details : pow_array);
   procedure Add_X_Axis_Year;
   procedure Add_Y_Axis;
   procedure Add_Y_Axis_Label   (X, Y : Integer; V : Float);
   procedure Add_Y_Axis_Name    (Y_String : String);
   function  Float_To_String    (f : Float; A : Integer) return String;
   function  Float_To_UnBounded (f : Float; A : Integer) return Ada.Strings.Unbounded.Unbounded_String;
   procedure Get_Graph_Extents  (Max : Long_Integer; ldivs : out graph_divs);
   function  Get_Min_Max        (power_details : pow_array) return Min_Max;
   function  Get_Min_Max_Day    (power_details : pow_array) return Min_Max;
   function  Get_Tooltip        (p : powerrec; uo : use_offset) return Unbounded_String;
   function  Get_Tooltip_Hour   (p1, p2 : powerrec) return Unbounded_String;
   function  Get_YBar           (p : Long_Integer) return Float;
   function  Integer_To_String  (I : Integer) return String;
end Construct_SVG;
