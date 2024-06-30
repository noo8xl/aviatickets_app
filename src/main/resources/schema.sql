-- drop tables before create a new one:
DROP TABLE IF EXISTS customer;
DROP TABLE IF EXISTS customer_details;
DROP TABLE IF EXISTS ticket;
DROP TABLE IF EXISTS ticket_details;
DROP TABLE IF EXISTS actions;
DROP TABLE IF EXISTS customer_orders;
DROP TABLE IF EXISTS customer_two_step_auth;


CREATE TABLE IF NOT EXISTS customer (
  id INT NOT NULL AUTO_INCREMENT,
  name varchar(30) NOT NULL,
  email varchar(250) NOT NULL,
  password varchar(30) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS customer_details (
  id INT NOT NULL AUTO_INCREMENT,
  created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  is_banned Boolean NOT NULL DEFAULT 0,
  role varchar(10) NOT NULL DEFAULT USER,
  customer_id INT NOT NULL,
  FOREIGN KEY (customer_id) REFERENCES customer (id),
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS customer_two_step_auth (
  id INT NOT NULL AUTO_INCREMENT,
  email varchar(250) NOT NULL,
  is_enabled Boolean NOT NULL DEFAULT 0,
  expired_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP() + 600000,
  code varchar(30) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS customer_orders (
  id INT NOT NULL AUTO_INCREMENT,
  created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  customer_id INT NOT NULL,
  FOREIGN KEY (customer_id) REFERENCES customer (id),
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS ticket (
  id INT NOT NULL AUTO_INCREMENT,


  customer_id INT NOT NULL,
  FOREIGN KEY (customer_id) REFERENCES customer (id),
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS ticket_details (
  id INT NOT NULL AUTO_INCREMENT,
  created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),

  ticket_id INT NOT NULL,
  FOREIGN KEY (ticket_id) REFERENCES ticket (id),
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS actions (
  id INT NOT NULL AUTO_INCREMENT,
  email varchar(250) NOT NULL,
  action_date timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
  action varchar(250) NOT NULL,
  customer_id INT NOT NULL,
  FOREIGN KEY (customer_id) REFERENCES customer (id),
  PRIMARY KEY (id)
);