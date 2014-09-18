TeamCity reports for EUnit
==========================

[![Build Status](https://travis-ci.org/deadok22/eunit_teamcity_reports.png)](https://travis-ci.org/deadok22/eunit_teamcity_reports)

EUnit formatter for TeamCity.


Installation & Usage
--------------------

Add eunit_teamcity dependency in your `erbar.config` and set report listener module option:

```erlang
{deps, [
    {eunit_teamcity, ".*",  {git, "https://github.com/deadok22/eunit_teamcity_reports.git", "master"}}
]}.

{eunit_opts, [no_tty, {report, {eunit_teamcity, []}}]}.
```
