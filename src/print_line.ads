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

with Record_Types;

package Print_Line is

   procedure Print_Line_Day    (t1 : Record_Types.tmrec);
   procedure Print_Line_Month  (t1 : Record_Types.tmrec; t2 : Record_Types.tmrec);
   procedure Print_Line_Year   (t1 : Record_Types.tmrec; t2 : Record_Types.tmrec; t3 : Record_Types.tmrec; t4 : Record_Types.tmrec);
   procedure Print_Power_Array (t1 : Record_Types.pow_array);
   procedure Print_Single_Line (s  : String);

end Print_Line;
