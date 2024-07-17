-- drop tables before create a new one:
DROP TABLE IF EXISTS customer;
DROP TABLE IF EXISTS customer_details;
DROP TABLE IF EXISTS ticket;
DROP TABLE IF EXISTS ticket_details;
DROP TABLE IF EXISTS actions;
DROP TABLE IF EXISTS customer_orders;
DROP TABLE IF EXISTS customer_two_step_auth;
DROP TABLE IF EXISTS airport;
DROP TABLE IF EXISTS airport_location;
DROP TABLE IF EXISTS aircraft;
DROP TABLE IF EXISTS aircraft_features;
DROP TABLE IF EXISTS cabin_class;
DROP TABLE IF EXISTS price_details;
DROP TABLE IF EXISTS baggage_allowance;

-- -------------------------------------------------------------------------------------

-- flights area
--

CREATE TABLE IF NOT EXISTS airport (
  id INT NOT NULL AUTO_INCREMENT,
  code varchar(10) NOT NULL,
  airport_name varchar(255) NOT NULL,
  city varchar(50) NOT NULL,
  country varchar(100) NOT NULL,
  terminal CHAR NOT NULL,
  timezone varchar(50) NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS airport_location (
	id INT NOT NULL AUTO_INCREMENT,
	longitude varchar(30) NOT NULL,
	latitude varchar(30) NOT NULL,
	altitude varchar(30),
	airport INT NOT NULL,
	FOREIGN KEY (airport) REFERENCES airport (id),
	PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS airport_contacts (
	id INT NOT NULL AUTO_INCREMENT,
	phone varchar(50) NOT NULL,
	email varchar(150) NOT NULL,
	website varchar(255) NOT NULL,
	airport INT NOT NULL,
	FOREIGN KEY (airport) REFERENCES airport (id),
	PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS aircraft (
  id INT NOT NULL AUTO_INCREMENT,
  model varchar(50) NOT NULL,
  registration varchar(50) NOT NULL,
  seating_capacity SMALLINT NOT NULL,
  year_of_manufacture SMALLINT NOT NULL,
  PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS aircraft_features (
  id INT NOT NULL AUTO_INCREMENT,
  wifi Boolean NOT NULL,
  entertainment Boolean NOT NULL,
  power_outlets Boolean NOT NULL,
	aircraft INT NOT NULL,
	FOREIGN KEY (aircraft) REFERENCES aircraft (id),
  PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS cabin_class (
  id INT NOT NULL AUTO_INCREMENT,
	economy Boolean NOT NULL,
  business Boolean NOT NULL,
  first Boolean NOT NULL,
  aircraft INT NOT NULL,
  FOREIGN KEY (aircraft) REFERENCES aircraft (id),
  PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS price_details (
  id INT NOT NULL AUTO_INCREMENT,
  currency varchar(5) NOT NULL DEFAULT "EUR",
  amount FLOAT NOT NULL,
	discount SMALLINT NOT NULL DEFAULT 0,
  baggage varchar(150) NOT NULL,
  PRIMARY KEY (id)
);

-- time format is hh:mm:ss ??*
CREATE TABLE IF NOT EXISTS leg_details (
	id INT NOT NULL AUTO_INCREMENT,

	departure_airport INT NOT NULL,
	arrival_airport INT NOT NULL,
	departure_time timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
	arrival_time timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
	duration varchar(15) NOT NULL,
	distance SMALLINT NOT NULL,
	status varchar(20) NOT NULL,

	FOREIGN KEY (departure_airport) REFERENCES airport (id),
	FOREIGN KEY (arrival_airport) REFERENCES airport (id),
	PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS flight_itinerary (
	id INT NOT NULL AUTO_INCREMENT,
	first_leg INT NOT NULL,
	second_leg INT,
	third_leg INT,
	fifth_leg INT,

	FOREIGN KEY (first_leg) REFERENCES leg_details (id),
	FOREIGN KEY (second_leg) REFERENCES leg_details (id),
	FOREIGN KEY (third_leg) REFERENCES leg_details (id),
	FOREIGN KEY (fifth_leg) REFERENCES leg_details (id),
	PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS flight (
  id INT NOT NULL AUTO_INCREMENT,
  flight_number varchar(50) NOT NULL,
  airline varchar(50) NOT NULL,

  itinerary INT NOT NULL,
  aircraft INT NOT NULL,

  distance SMALLINT NOT NULL,
  total_duration varchar(30) NOT NULL,
  price INT NOT NULL,
  passenger_count SMALLINT NOT NULL,
	availableSits SMALLINT NOT NULL,

	FOREIGN KEY (itinerary) REFERENCES flight_itinerary (id),
  FOREIGN KEY (price) REFERENCES price_details (id),
  FOREIGN KEY (aircraft) REFERENCES aircraft (id),

  PRIMARY KEY (id)
);

-- -------------------------------------------------------------------------------------

-- customer area
--

-- CREATE TABLE IF NOT EXISTS customer (
--   id INT NOT NULL AUTO_INCREMENT,
--   name varchar(30) NOT NULL,
--   email varchar(250) NOT NULL,
--   password varchar(30) NOT NULL,
--   PRIMARY KEY (id)
-- );
--
-- CREATE TABLE IF NOT EXISTS customer_details (
--   id INT NOT NULL AUTO_INCREMENT,
--   created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
--   updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
--   is_banned Boolean NOT NULL DEFAULT 0,
--   role varchar(10) NOT NULL DEFAULT USER,
--   customer_id INT NOT NULL,
--   FOREIGN KEY (customer_id) REFERENCES customer (id),
--   PRIMARY KEY (id)
-- );
--
-- CREATE TABLE IF NOT EXISTS customer_two_step_auth (
--   id INT NOT NULL AUTO_INCREMENT,
--   email varchar(250) NOT NULL,
--   is_enabled Boolean NOT NULL DEFAULT 0,
--   expired_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP() + 600000,
--   code varchar(30) NOT NULL,
--   PRIMARY KEY (id)
-- );
--
-- CREATE TABLE IF NOT EXISTS customer_orders (
--   id INT NOT NULL AUTO_INCREMENT,
--   created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
--   updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
--   customer_id INT NOT NULL,
--   FOREIGN KEY (customer_id) REFERENCES customer (id),
--   PRIMARY KEY (id)
-- );
--
-- CREATE TABLE IF NOT EXISTS ticket (
--   id INT NOT NULL AUTO_INCREMENT,
--
--
--   customer_id INT NOT NULL,
--   FOREIGN KEY (customer_id) REFERENCES customer (id),
--   PRIMARY KEY (id)
-- );
--
-- CREATE TABLE IF NOT EXISTS ticket_details (
--   id INT NOT NULL AUTO_INCREMENT,
--   created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
--   updated_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
--
--   ticket_id INT NOT NULL,
--   FOREIGN KEY (ticket_id) REFERENCES ticket (id),
--   PRIMARY KEY (id)
-- );
--
-- CREATE TABLE IF NOT EXISTS actions (
--   id INT NOT NULL AUTO_INCREMENT,
--   email varchar(250) NOT NULL,
--   action_date timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP(),
--   action varchar(250) NOT NULL,
--   customer_id INT NOT NULL,
--   FOREIGN KEY (customer_id) REFERENCES customer (id),
--   PRIMARY KEY (id)
-- );

-- end of
-- customer area
--

-- -------------------------------------------------------------------------------------