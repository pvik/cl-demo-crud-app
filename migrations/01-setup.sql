CREATE DATABASE sample_crud;

\c sample_crud

CREATE SCHEMA sample;

CREATE TABLE sample.todo (
	   id serial,
	   item varchar,
	   note text,
	   completed boolean,
	   create_date timestamptz,
	   completed_date timestamptz
);
