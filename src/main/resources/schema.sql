-- drop tables before create a new one:
DROP TABLE IF EXISTS customer;
DROP TABLE IF EXISTS customer_details;
DROP TABLE IF EXISTS ticket;
DROP TABLE IF EXISTS ticket_details;
DROP TABLE IF EXISTS actions;
DROP TABLE IF EXISTS customer_orders;


CREATE TABLE IF NOT EXISTS customer (
  id INT NOT NULL AUTO_INCREMENT,
  name varchar(30) NOT NULL,
  email varchar(250) NOT NULL,
  password varchar(30) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS customer_details (
  id INT NOT NULL AUTO_INCREMENT,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  is_activated Boolean NOT NULL,
  role varchar(10) NOT NULL,
  customer_id INT NOT NULL,
  FOREIGN KEY (customer_id) REFERENCES customer (id),
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS customer_orders (
  id INT NOT NULL AUTO_INCREMENT,
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  role varchar(10) NOT NULL,
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
  created_at timestamp NOT NULL,
  updated_at timestamp NOT NULL,
  role Boolean NOT NULL,
  ticket_id INT NOT NULL,
  FOREIGN KEY (ticket_id) REFERENCES ticket (id),
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS actions (
  id INT NOT NULL AUTO_INCREMENT,
  email varchar(250) NOT NULL,
  action_date timestamp NOT NULL,
  action varchar(250) NOT NULL,
  customer_id INT NOT NULL,
  FOREIGN KEY (customer_id) REFERENCES customer (id),
  PRIMARY KEY (id)
);