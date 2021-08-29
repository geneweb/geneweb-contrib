#!/bin/bash

# ========== MySQL only ==============
# To store login / password outside :
# mysql_config_editor set --login-path=client --user=insee --password
#mysql insee $@

# ========== MariaDB only ==============
mysql insee -uinsee -pinsee_pw $@
