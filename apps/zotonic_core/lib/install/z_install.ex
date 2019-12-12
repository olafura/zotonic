defmodule :z_install do
  _untranslated = "** 47: can't find include lib \"zotonic.hrl\" **"

  def install(context) do
    :ok =
      :z_db.transaction(
        fn context1 ->
          :ok = install_sql_list(context1, model_pgsql())
          :ok = :z_install_data.install(:z_context.site(context1), context1)
        end,
        context
      )
  end

  def install_sql_list(context, model) do
    c = :z_db_pgsql.get_raw_connection(context)
    for sql <- model, do: {:ok, [], []} = :epgsql.squery(c, sql)
    :ok
  end

  def model_pgsql() do
    [
      'CREATE TABLE config\n    (\n      id serial NOT NULL,\n      module character varying(80) NOT NULL DEFAULT \'zotonic\'::character varying,\n      key character varying(80) NOT NULL DEFAULT \'\'::character varying,\n      value text NOT NULL DEFAULT \'\'::character varying,\n      props bytea,\n      created timestamp with time zone NOT NULL DEFAULT now(),\n      modified timestamp with time zone NOT NULL DEFAULT now(),\n\n      CONSTRAINT config_pkey PRIMARY KEY (id),\n      CONSTRAINT config_module_key_key UNIQUE (module, key)\n    )',
      'CREATE TABLE module\n    (\n      id serial NOT NULL,\n      name character varying(80) NOT NULL DEFAULT \'\'::character varying,\n      uri character varying(2048) NOT NULL DEFAULT \'\'::character varying,\n      is_active boolean NOT NULL DEFAULT false,\n      created timestamp with time zone NOT NULL DEFAULT now(),\n      modified timestamp with time zone NOT NULL DEFAULT now(),\n      schema_version int NULL,\n\n      CONSTRAINT module_pkey PRIMARY KEY (id),\n      CONSTRAINT module_name_key UNIQUE (name)\n    )',
      'CREATE TABLE rsc\n    (\n      id serial NOT NULL,\n      uri character varying(2048),\n      name character varying(80),\n      page_path character varying(80),\n      is_authoritative boolean NOT NULL DEFAULT true,\n      is_published boolean NOT NULL DEFAULT false,\n      is_featured boolean NOT NULL DEFAULT false,\n      is_protected boolean NOT NULL DEFAULT false,\n      is_dependent boolean NOT NULL DEFAULT false,\n      publication_start timestamp with time zone,\n      publication_end timestamp with time zone NOT NULL DEFAULT \'9999-06-01 00:00:00\'::timestamp with time zone,\n      content_group_id int,\n      creator_id int,\n      modifier_id int,\n      version int NOT NULL DEFAULT 1,\n      category_id int NOT NULL,\n      visible_for int NOT NULL DEFAULT 0, -- 0 = public, > 1 defined by ACL module\n      slug character varying(80) NOT NULL DEFAULT \'\'::character varying,\n      props bytea,\n      created timestamp with time zone NOT NULL DEFAULT now(),\n      modified timestamp with time zone NOT NULL DEFAULT now(),\n\n      -- pivot fields for searching\n      pivot_category_nr int,\n      pivot_tsv tsvector,       -- texts\n      pivot_rtsv tsvector,      -- related ids (cat, prop, rsc)\n\n      pivot_first_name character varying(100),\n      pivot_surname character varying(100),\n      pivot_gender character varying(1),\n\n      pivot_date_start timestamp with time zone,\n      pivot_date_end timestamp with time zone,\n      pivot_date_start_month_day int,  -- used for birthdays\n      pivot_date_end_month_day int,    -- used for decease dates\n\n      pivot_street character varying(120),\n      pivot_city character varying(100),\n      pivot_state character varying(50),\n      pivot_postcode character varying(30),\n      pivot_country character varying(80),\n      pivot_geocode bigint,\n      pivot_geocode_qhash bytea,\n      pivot_title character varying(100),\n\n      pivot_location_lat float,\n      pivot_location_lng float,\n\n      CONSTRAINT rsc_pkey PRIMARY KEY (id),\n      CONSTRAINT rsc_uri_key UNIQUE (uri),\n      CONSTRAINT rsc_name_key UNIQUE (name),\n      CONSTRAINT rsc_page_path_key UNIQUE (page_path)\n    )',
      'ALTER TABLE rsc ADD CONSTRAINT fk_rsc_content_group_id FOREIGN KEY (content_group_id)\n      REFERENCES rsc (id)\n      ON UPDATE CASCADE ON DELETE SET NULL',
      'ALTER TABLE rsc ADD CONSTRAINT fk_rsc_creator_id FOREIGN KEY (creator_id)\n      REFERENCES rsc (id)\n      ON UPDATE CASCADE ON DELETE SET NULL',
      'ALTER TABLE rsc ADD CONSTRAINT fk_rsc_modifier_id FOREIGN KEY (modifier_id)\n      REFERENCES rsc (id)\n      ON UPDATE CASCADE ON DELETE SET NULL',
      'CREATE INDEX fki_rsc_content_group_id ON rsc (content_group_id)',
      'CREATE INDEX fki_rsc_creator_id ON rsc (creator_id)',
      'CREATE INDEX fki_rsc_modifier_id ON rsc (modifier_id)',
      'CREATE INDEX fki_rsc_created ON rsc (created)',
      'CREATE INDEX fki_rsc_modified ON rsc (modified)',
      'CREATE INDEX rsc_pivot_tsv_key ON rsc USING gin(pivot_tsv)',
      'CREATE INDEX rsc_pivot_rtsv_key ON rsc USING gin(pivot_rtsv)',
      'CREATE INDEX rsc_pivot_category_nr ON rsc (pivot_category_nr)',
      'CREATE INDEX rsc_pivot_surname_key ON rsc (pivot_surname)',
      'CREATE INDEX rsc_pivot_first_name_key ON rsc (pivot_first_name)',
      'CREATE INDEX rsc_pivot_gender_key ON rsc (pivot_gender)',
      'CREATE INDEX rsc_pivot_date_start_key ON rsc (pivot_date_start)',
      'CREATE INDEX rsc_pivot_date_end_key ON rsc (pivot_date_end)',
      'CREATE INDEX rsc_pivot_date_start_month_day_key ON rsc (pivot_date_start_month_day)',
      'CREATE INDEX rsc_pivot_date_end_month_day_key ON rsc (pivot_date_end_month_day)',
      'CREATE INDEX rsc_pivot_city_street_key ON rsc (pivot_city, pivot_street)',
      'CREATE INDEX rsc_pivot_country_key ON rsc (pivot_country)',
      'CREATE INDEX rsc_pivot_postcode_key ON rsc (pivot_postcode)',
      'CREATE INDEX rsc_pivot_geocode_key ON rsc (pivot_geocode)',
      'CREATE INDEX rsc_pivot_title_key ON rsc (pivot_title)',
      'CREATE INDEX rsc_pivot_location_key ON rsc (pivot_location_lat, pivot_location_lng)',
      'CREATE TABLE rsc_gone (\n        id bigint not null,\n     new_id bigint,\n     new_uri character varying(2048),\n     version int not null,\n     uri character varying(2048),\n     name character varying(80),\n     page_path character varying(80),\n     is_authoritative boolean NOT NULL DEFAULT true,\n     creator_id bigint,\n     modifier_id bigint,\n     created timestamp with time zone NOT NULL DEFAULT now(),\n     modified timestamp with time zone NOT NULL DEFAULT now(),\n     CONSTRAINT rsc_gone_pkey PRIMARY KEY (id)\n    )',
      'CREATE INDEX rsc_gone_name ON rsc_gone(name)',
      'CREATE INDEX rsc_gone_page_path ON rsc_gone(page_path)',
      'CREATE INDEX rsc_gone_modified ON rsc_gone(modified)',
      'CREATE TABLE protect (\n        id int NOT NULL,\n\n    CONSTRAINT protect_id PRIMARY KEY (id),\n    CONSTRAINT fk_protect_id FOREIGN KEY (id)\n        REFERENCES rsc(id)\n        ON UPDATE CASCADE ON DELETE RESTRICT\n        )',
      'CREATE TABLE edge\n    (\n      id serial NOT NULL,\n      subject_id int NOT NULL,\n      predicate_id int NOT NULL,\n      object_id int NOT NULL,\n      seq int NOT NULL DEFAULT 1000000,\n      creator_id int,\n      created timestamp with time zone NOT NULL DEFAULT now(),\n\n      CONSTRAINT edge_pkey PRIMARY KEY (id),\n      CONSTRAINT edge_ops_key UNIQUE (object_id, predicate_id, subject_id),\n      CONSTRAINT edge_spo_key UNIQUE (subject_id, predicate_id, object_id),\n      CONSTRAINT fk_edge_subject_id FOREIGN KEY (subject_id)\n        REFERENCES rsc (id)\n        ON UPDATE CASCADE ON DELETE CASCADE,\n      CONSTRAINT fk_edge_object_id FOREIGN KEY (object_id)\n        REFERENCES rsc (id)\n        ON UPDATE CASCADE ON DELETE CASCADE,\n      CONSTRAINT fk_edge_predicate_id FOREIGN KEY (predicate_id)\n        REFERENCES rsc (id)\n        ON UPDATE CASCADE ON DELETE CASCADE,\n      CONSTRAINT fk_edge_creator_id FOREIGN KEY (creator_id)\n        REFERENCES rsc (id)\n        ON UPDATE CASCADE ON DELETE SET NULL\n    )',
      'CREATE INDEX fki_edge_subject_id ON edge (subject_id)',
      'CREATE INDEX fki_edge_predicate_id ON edge (predicate_id)',
      'CREATE INDEX fki_edge_object_id ON edge (object_id)',
      'CREATE INDEX fki_edge_creator_id ON edge (creator_id)',
      'CREATE INDEX edge_sp_seq_key ON edge (subject_id, predicate_id, seq)',
      'CREATE TABLE medium\n    (\n      id int NOT NULL,\n      filename character varying(400),\n      rootname character varying(100),\n      mime character varying(128) NOT NULL DEFAULT \'application/octet-stream\'::character varying,\n      width int NOT NULL DEFAULT 0,\n      height int NOT NULL DEFAULT 0,\n      orientation int NOT NULL DEFAULT 1,\n      sha1 character varying(40),\n      size int NOT NULL DEFAULT 0,\n      preview_filename character varying(400),\n      preview_width int NOT NULL DEFAULT 0,\n      preview_height int NOT NULL DEFAULT 0,\n      is_deletable_file boolean NOT NULL DEFAULT false,\n      is_deletable_preview boolean NOT NULL DEFAULT false,\n      props bytea,\n      created timestamp with time zone NOT NULL DEFAULT now(),\n\n      CONSTRAINT medium_pkey PRIMARY KEY (id),\n      CONSTRAINT medium_filename_key UNIQUE (filename),\n      CONSTRAINT fk_medium_rsc_id FOREIGN KEY (id)\n        REFERENCES rsc (id)\n        ON UPDATE CASCADE ON DELETE CASCADE\n    )',
      'CREATE INDEX medium_rootname_key ON medium (rootname)',
      'CREATE TABLE predicate_category\n    (\n      id serial NOT NULL,\n      is_subject boolean NOT NULL DEFAULT true,\n      predicate_id int NOT NULL,\n      category_id int NOT NULL,\n\n      CONSTRAINT predicate_category_pkey PRIMARY KEY (id),\n      CONSTRAINT predicate_category_key UNIQUE (predicate_id, is_subject, category_id),\n      CONSTRAINT fk_predicate_category_predicate_id FOREIGN KEY (predicate_id)\n        REFERENCES rsc(id)\n        ON UPDATE CASCADE\n        ON DELETE CASCADE,\n      CONSTRAINT fk_predicate_category_category_id FOREIGN KEY (category_id)\n        REFERENCES rsc(id)\n        ON UPDATE CASCADE\n        ON DELETE CASCADE\n    )',
      'CREATE INDEX fki_predicate_category_predicate_id ON predicate_category (predicate_id)',
      'CREATE INDEX fki_predicate_category_category_id ON predicate_category (category_id)',
      'CREATE TABLE identity\n    (\n      id serial NOT NULL,\n      rsc_id int NOT NULL,\n      type character varying(32) NOT NULL DEFAULT \'\'::character varying,\n      key character varying(200) NOT NULL DEFAULT \'\'::character varying,\n      is_unique boolean,          -- set to true when the type/key should be unique\n      is_verified boolean not null default false,\n      verify_key character varying(32),\n      propb bytea,\n      prop1 character varying(200) NOT NULL DEFAULT \'\'::character varying,\n      prop2 character varying(200) NOT NULL DEFAULT \'\'::character varying,\n      prop3 character varying(200) NOT NULL DEFAULT \'\'::character varying,\n      created timestamp with time zone NOT NULL DEFAULT now(),\n      modified timestamp with time zone NOT NULL DEFAULT now(),\n      visited timestamp with time zone,\n\n      CONSTRAINT auth_pkey PRIMARY KEY (id),\n      CONSTRAINT pk_auth_rsc_id FOREIGN KEY (rsc_id)\n        REFERENCES rsc (id)\n        ON UPDATE CASCADE ON DELETE CASCADE,\n      CONSTRAINT identity_type_key_unique UNIQUE (type, key, is_unique),\n      CONSTRAINT identity_verify_key_unique UNIQUE (verify_key)\n    )',
      'CREATE INDEX fki_identity_rsc_id ON identity (rsc_id)',
      'CREATE INDEX identity_type_key_key ON identity (type, key)',
      'CREATE INDEX identity_visited_key ON identity (visited)',
      'CREATE INDEX identity_created_key ON identity (created)',
      'CREATE TABLE emailq (\n        id serial NOT NULL,\n        status character varying(10) not null default \'new\', -- new, sent, fail\n        retry_on timestamp with time zone default (now() + \'00:10:00\'::interval),\n        retry int not null default 0,\n        sender character varying(100),\n        recipient character varying(100),\n        props bytea,\n        sent timestamp with time zone,\n        created timestamp with time zone not null default now(),\n        CONSTRAINT email_pkey PRIMARY KEY (id)\n    )',
      'CREATE INDEX email_recipient_key ON emailq (recipient)',
      'CREATE INDEX email_created_key ON emailq (created)',
      'CREATE INDEX email_status_retry_key ON emailq (status, retry_on)',
      'CREATE TABLE rsc_pivot_queue\n    (\n        rsc_id int NOT NULL,\n        serial int NOT NULL DEFAULT 1,\n        due timestamp with time zone,\n        is_update boolean NOT NULL default true,\n\n        CONSTRAINT rsc_pivot_queue_pkey PRIMARY KEY (rsc_id),\n        CONSTRAINT fk_rsc_pivot_queue_rsc_id FOREIGN KEY (rsc_id)\n          REFERENCES rsc(id)\n          ON UPDATE CASCADE ON DELETE CASCADE\n    )',
      'CREATE INDEX fki_rsc_pivot_queue_rsc_id ON rsc_pivot_queue (rsc_id)',
      'CREATE INDEX fki_rsc_pivot_queue_due ON rsc_pivot_queue (is_update, due)',
      'CREATE TABLE pivot_task_queue\n    (\n        id serial NOT NULL,\n        module character varying(80) NOT NULL,\n        function character varying(64) NOT NULL,\n        key character varying(100) NOT NULL DEFAULT \'\'::character varying,\n        due timestamp with time zone ,\n        props bytea,\n\n        CONSTRAINT pivot_task_queue_pkey PRIMARY KEY (id),\n        CONSTRAINT pivot_task_queue_module_funcion_key_key UNIQUE (module, function, key)\n    )\n    ',
      '\n    CREATE FUNCTION rsc_pivot_update() RETURNS trigger AS $$\n    declare\n        duetime timestamp;\n        do_queue boolean;\n    begin\n        if (tg_op = \'INSERT\') then\n            do_queue := true;\n        elseif (new.version <> old.version or new.modified <> old.modified) then\n            do_queue := true;\n        else\n            do_queue := false;\n        end if;\n\n        if (do_queue) then\n            <<insert_update_queue>>\n            loop\n                update rsc_pivot_queue\n                set due = (case when now() < due then now() else due end),\n                    serial = serial + 1\n                where rsc_id = new.id;\n\n                exit insert_update_queue when found;\n\n                begin\n                    insert into rsc_pivot_queue (rsc_id, due, is_update) values (new.id, now(), tg_op = \'UPDATE\');\n                    exit insert_update_queue;\n                exception\n                    when unique_violation then\n                        -- do nothing\n                end;\n            end loop insert_update_queue;\n        end if;\n\n        if (new.is_protected) then\n            begin\n                insert into protect (id) values (new.id);\n            exception\n                when unique_violation then\n                    -- do nothing\n            end;\n        else\n            delete from protect where id = new.id;\n        end if;\n        return null;\n    end;\n    $$ LANGUAGE plpgsql\n    ',
      '\n    CREATE TRIGGER rsc_update_queue_trigger AFTER INSERT OR UPDATE\n    ON rsc FOR EACH ROW EXECUTE PROCEDURE rsc_pivot_update()\n    ',
      'CREATE TABLE medium_deleted\n    (\n        id serial NOT NULL,\n        filename character varying (400) NOT NULL,\n        deleted timestamp with time zone NOT NULL default now(),\n\n        CONSTRAINT medium_deleted_pkey PRIMARY KEY (id)\n    )',
      'CREATE INDEX medium_deleted_deleted_key ON medium_deleted (deleted)',
      '\n    CREATE FUNCTION medium_delete() RETURNS trigger AS $$\n    begin\n        if (tg_op = \'DELETE\') then\n            if (old.filename <> \'\' and old.filename is not null and old.is_deletable_file) then\n                insert into medium_deleted (filename) values (old.filename);\n            end if;\n            if (old.preview_filename <> \'\' and old.preview_filename is not null and old.is_deletable_preview) then\n                insert into medium_deleted (filename) values (old.preview_filename);\n            end if;\n        end if;\n        return null;\n    end;\n    $$ LANGUAGE plpgsql\n    ',
      '\n    CREATE TRIGGER medium_deleted_trigger AFTER DELETE\n    ON medium FOR EACH ROW EXECUTE PROCEDURE medium_delete()\n    ',
      hierarchy_table(),
      hierarchy_index_1(),
      hierarchy_index_2(),
      medium_log_table(),
      medium_update_function(),
      medium_update_trigger(),
      rsc_page_path_log(),
      rsc_page_path_log_fki(),
      edge_log_table(),
      edge_log_function(),
      edge_log_trigger()
    ]
  end

  def hierarchy_table do
    'CREATE TABLE hierarchy (\n        name character varying (80),\n        id int NOT NULL,\n        parent_id int,\n        nr int NOT NULL DEFAULT 0,\n        lvl int NOT NULL DEFAULT 0,\n        lft int NOT NULL DEFAULT 0,\n        rght int NOT NULL DEFAULT 0,\n\n        CONSTRAINT hierarchy_pkey PRIMARY KEY (name, id),\n        CONSTRAINT fk_hierarchy_id FOREIGN KEY (id)\n          REFERENCES rsc(id)\n          ON UPDATE CASCADE ON DELETE CASCADE\n          DEFERRABLE INITIALLY DEFERRED\n    )'
  end

  def hierarchy_index_1 do
    'CREATE INDEX hierarchy_nr_key ON hierarchy (name, nr)'
  end

  def hierarchy_index_2 do
    'CREATE INDEX fki_hierarchy_id ON hierarchy (id)'
  end

  def edge_log_table do
    'CREATE TABLE edge_log\n    (\n        id bigserial NOT NULL,\n        op character varying(6),\n        edge_id int not null,\n        subject_id int not null,\n        predicate_id int not null,\n        predicate character varying (80),\n        object_id int not null,\n        seq integer not null,\n        logged timestamp with time zone NOT NULL default now(),\n        created timestamp with time zone,\n\n        CONSTRAINT edge_log_pkey PRIMARY KEY (id)\n    )'
  end

  def edge_log_function do
    '\n    CREATE OR REPLACE FUNCTION edge_update() RETURNS trigger AS $$\n    declare\n        new_predicate character varying(80);\n    begin\n        if (tg_op = \'INSERT\') then\n            select into new_predicate r.name from rsc r where r.id = new.predicate_id;\n            insert into edge_log (op, edge_id, subject_id, object_id, predicate_id, predicate, seq)\n            values (tg_op, new.id, new.subject_id, new.object_id, new.predicate_id, new_predicate, new.seq);\n        elseif (tg_op = \'UPDATE\') then\n            select into new_predicate r.name from rsc r where r.id = new.predicate_id;\n            insert into edge_log (op, edge_id, subject_id, object_id, predicate_id, predicate, seq)\n            values (tg_op, new.id, new.subject_id, new.object_id, new.predicate_id, new_predicate, new.seq);\n        elseif (tg_op = \'DELETE\') then\n            select into new_predicate r.name from rsc r where r.id = old.predicate_id;\n            insert into edge_log (op, edge_id, subject_id, object_id, predicate_id, predicate, seq, created)\n            values (tg_op, old.id, old.subject_id, old.object_id, old.predicate_id, new_predicate, old.seq, old.created);\n        end if;\n        return null;\n    end;\n    $$ LANGUAGE plpgsql\n    '
  end

  def edge_log_trigger do
    '\n    CREATE TRIGGER edge_update_trigger AFTER INSERT OR UPDATE OR DELETE\n    ON edge FOR EACH ROW EXECUTE PROCEDURE edge_update()\n    '
  end

  def medium_log_table do
    'CREATE TABLE medium_log\n    (\n        id serial NOT NULL,\n        usr_id int,\n        filename character varying (400) NOT NULL,\n        created timestamp with time zone NOT NULL default now(),\n\n        CONSTRAINT medium_log_pkey PRIMARY KEY (id),\n        CONSTRAINT medium_log_filename_key UNIQUE (filename)\n    )'
  end

  def medium_update_function do
    '\n    CREATE FUNCTION medium_update() RETURNS trigger AS $$\n    declare\n        user_id integer;\n    begin\n        select into user_id r.creator_id from rsc r where r.id = new.id;\n        if (tg_op = \'INSERT\') then\n            if (new.filename <> \'\' and new.filename is not null and new.is_deletable_file) then\n                insert into medium_log (filename, usr_id)\n                values (new.filename, user_id);\n            end if;\n            if (new.preview_filename <> \'\' and new.preview_filename is not null and new.is_deletable_preview) then\n                insert into medium_log (filename, usr_id)\n                values (new.preview_filename, user_id);\n            end if;\n        elseif (tg_op = \'UPDATE\') then\n            if (new.filename <> \'\' and new.filename is not null and new.is_deletable_file and new.filename != old.filename) then\n                insert into medium_log (filename, usr_id)\n                values (new.filename, user_id);\n            end if;\n            if (new.preview_filename <> \'\' and new.preview_filename is not null and new.is_deletable_preview and new.preview_filename != old.preview_filename) then\n                insert into medium_log (filename, usr_id)\n                values (new.preview_filename, user_id);\n            end if;\n            -- Insert files into the medium_deleted queue table\n            if (old.filename <> \'\' and old.filename is not null and old.is_deletable_file and new.filename != old.filename) then\n                insert into medium_deleted (filename) values (old.filename);\n            end if;\n            if (old.preview_filename <> \'\' and old.preview_filename is not null and old.is_deletable_preview and new.preview_filename != old.preview_filename) then\n                insert into medium_deleted (filename) values (old.preview_filename);\n            end if;\n        end if;\n        return null;\n    end;\n    $$ LANGUAGE plpgsql\n    '
  end

  def medium_update_trigger do
    '\n    CREATE TRIGGER medium_update_trigger AFTER INSERT OR UPDATE\n    ON medium FOR EACH ROW EXECUTE PROCEDURE medium_update()\n    '
  end

  def rsc_page_path_log do
    'CREATE TABLE rsc_page_path_log (\n      page_path character varying(80),\n      id int not null,\n      created timestamp with time zone NOT NULL DEFAULT now(),\n      CONSTRAINT rsc_page_path_log_pkey PRIMARY KEY (page_path),\n      CONSTRAINT fk_rsc_page_path_log_id FOREIGN KEY (id)\n        REFERENCES rsc(id)\n        ON UPDATE CASCADE ON DELETE CASCADE\n    )'
  end

  def rsc_page_path_log_fki do
    'CREATE INDEX fki_rsc_page_path_log_id ON rsc_page_path_log (id)'
  end
end
