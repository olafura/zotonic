#!/bin/sh

# If the command given is a zotonic command, pass it to zotonic; otherwise exec it directly.
# Also check the environment for "FORCE_ZOTONIC" to provide a workaround in case the scripts 
# are moved somewhere outside of the path below.
if [ -e "/opt/zotonic/apps/zotonic_launcher/src/command/zotonic_cmd_$1.erl" ] || [ -n "$FORCE_ZOTONIC" ]; then
    set -x

    HOME=/tmp
    ZOTONIC_PIDFILE=/tmp/zotonic.pid
    ZOTONIC_CONFIG_DIR=/etc/zotonic
    SHELL=/bin/sh

    export HOME ZOTONIC_PIDFILE ZOTONIC_CONFIG_DIR SHELL

    # Create the pid file and enable zotonic to write to it
    touch /run/zotonic.pid && chown zotonic /run/zotonic.pid

    # Allow zotonic to write some state
    mkdir /opt/zotonic/priv && chown -R zotonic /opt/zotonic/priv

    # The mimetypes app writes here on startup
    chown -R zotonic /opt/zotonic/_build/default/lib/mimetypes/ebin

    # The status site gets ssl keys written to its priv dir
    chown -R zotonic /opt/zotonic/_build/default/lib/zotonic_site_status/priv/

    # Insert password from environment variable into zotonic.config
    sed -i -e "s/{password, \"\"}/{password, \"${ZOTONIC_PASSWORD}\"}/" /etc/zotonic/zotonic.config

    exec /usr/bin/gosu zotonic /opt/zotonic/bin/zotonic "$@"
else
    exec "$@"
fi
