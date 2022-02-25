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

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package String_Stream is

   procedure Open
      (Stream : in out String_Stream_Type'Class;
       Str    : String);

   procedure Read
     (Stream : in out String_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   type String_Stream_Type is new Root_Stream_Type with record
      Str        : Unbounded_String;
      Read_Index : Natural := 1;
   end record;

   procedure Write
      (Stream : in out String_Stream_Type;
       Item   : Stream_Element_Array);
   --  Declare a new stream type to write strings in memory

end String_Stream;
