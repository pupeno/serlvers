%%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%%
%%% This file is part of Serlvers.
%%%
%%% Serlvers is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%%% Serlvers is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%%% You should have received a copy of the GNU General Public License along with Serlvers; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%%% Linking Serlvers statically or dynamically with other modules is making a combined work based on Serlvers. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%%% In addition, as a special exception, the copyright holders of Serlvers give you permission to combine Serlvers program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for Serlvers and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

%%% @author José Pablo Ezequiel "Pupeno" Fernández Silva <pupeno@pupeno.com> [http://pupeno.com]
%%% @copyright 2006 José Pablo Ezequiel "Pupeno" Fernández Silva

{application, serlvers,
 [{description, "Library for making servers implementing the networking part of application level servers (dns, smtp, pop3... some day)."},
  {vsn, "0.1.0"},
  {modules, [launcher, gen_echo, gen_daytime]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
