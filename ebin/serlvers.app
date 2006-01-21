{application, serlvers,
 [{description, "Library for making servers implementing the networking part of application level servers (dns, smtp, pop3... some day)."},
  {vsn, "0.0.0"},
  {modules, [launcher, gen_echo, gen_daytime]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]}]}.
