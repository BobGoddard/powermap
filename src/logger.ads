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

with Ada.Exceptions;
with Ada.Strings.Unbounded;

package Logger is
   procedure Initialise;
   protected Secure is
      procedure Logger (S : String);
      procedure Logger (S : Ada.Strings.Unbounded.Unbounded_String);
      procedure Logger (S : String;                                 E : Ada.Exceptions.Exception_Occurrence);
      procedure Logger (S : Ada.Strings.Unbounded.Unbounded_String; E : Ada.Exceptions.Exception_Occurrence);
   end Secure;
end Logger;
