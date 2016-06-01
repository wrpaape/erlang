{application,hash_table,
             [{description,"Erlang Baby's 1st Hash Table"},
              {vsn,"1.0"},
              {registered,[hash_table_app,hash_table_sup,hash_table]},
              {applications,[kernel,stdlib,sasl]},
              {mod,{hash_table_app,[]}},
              {env,[{default_initial_capacity,1024}]},
              {modules,[hash_table,hash_table_app,hash_table_sup]}]}.
