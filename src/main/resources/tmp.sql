


DROP VIEW IF EXISTS GET_DETAILED_FLIGHT;
DROP PROCEDURE IF EXISTS create_flight_details_view;

DELIMITER $$

CREATE PROCEDURE IF NOT EXISTS create_flight_details_view(
    IN flightNumber VARCHAR(50)
)
BEGIN



    CREATE VIEW GET_DETAILED_FLIGHT AS
        SELECT
            flights.flight_number, flights.airline,
            FROM flights
            WHERE flight_number = flightNumber;


END $$

DELIMITER ;


CALL create_flight_details_view('LH2001');

SELECT * FROM GET_DETAILED_FLIGHT;

















-- flights area
--

CREATE TABLE IF NOT EXISTS airport (
               id INT NOT NULL AUTO_INCREMENT,
               code varchar(10) NOT NULL,
               airport_name varchar(255) NOT NULL,
               city varchar(50) NOT NULL,
               country varchar(100) NOT NULL,
               terminal varchar(5) NOT NULL,
               timezone varchar(50) NOT NULL,
               PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS airport_location (
                                                id INT NOT NULL AUTO_INCREMENT,
                                                longitude varchar(30) NOT NULL,
                                                latitude varchar(30) NOT NULL,
                                                altitude varchar(30),
                                                airport_id INT NOT NULL,
                                                FOREIGN KEY (airport_id) REFERENCES airport (id),
                                                PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS airport_contacts (
                                                id INT NOT NULL AUTO_INCREMENT,
                                                phone varchar(50) NOT NULL,
                                                email varchar(150) NOT NULL,
                                                website varchar(255) NOT NULL,
                                                airport_id INT NOT NULL,
                                                FOREIGN KEY (airport_id) REFERENCES airport (id),
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
                                                 aircraft_id INT NOT NULL,
                                                 FOREIGN KEY (aircraft_id) REFERENCES aircraft (id),
                                                 PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS cabin_class (
                                           id INT NOT NULL AUTO_INCREMENT,
                                           economy Boolean NOT NULL,
                                           business Boolean NOT NULL,
                                           first Boolean NOT NULL,
                                           aircraft_id INT NOT NULL,
                                           FOREIGN KEY (aircraft_id) REFERENCES aircraft (id),
                                           PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS price_details (
                                             id INT NOT NULL AUTO_INCREMENT,
                                             flight_number varchar(50) NOT NULL,
                                             currency varchar(5) NOT NULL DEFAULT 'EUR',
                                             amount FLOAT NOT NULL,
                                             discount SMALLINT NOT NULL DEFAULT 0,
                                             baggage varchar(150) NOT NULL,
                                             PRIMARY KEY (id)
);

-- time format is hh:mm:ss ??*
CREATE TABLE IF NOT EXISTS leg_details (
                                           id INT NOT NULL AUTO_INCREMENT,

                                           flight_number varchar(50) NOT NULL,
                                           departure_airport INT NOT NULL,
                                           arrival_airport INT NOT NULL,
                                           departure_time timestamp NOT NULL,
                                           arrival_time timestamp NOT NULL,
                                           duration varchar(15) NOT NULL,
                                           distance SMALLINT NOT NULL,
                                           status varchar(20) NOT NULL,

                                           FOREIGN KEY (departure_airport) REFERENCES airport (id),
                                           FOREIGN KEY (arrival_airport) REFERENCES airport (id),
                                           PRIMARY KEY (id)
);


CREATE TABLE IF NOT EXISTS flights (
                                       id INT NOT NULL AUTO_INCREMENT,

                                       flight_number varchar(50) NOT NULL,
                                       airline varchar(50) NOT NULL,
                                       aircraft_id INT NOT NULL,

                                       departure_time timestamp NOT NULL,
                                       distance SMALLINT NOT NULL,
                                       total_duration varchar(30) NOT NULL,
                                       price INT NOT NULL,
                                       passenger_count SMALLINT NOT NULL,
                                       available_sits SMALLINT NOT NULL,

                                       FOREIGN KEY (price) REFERENCES price_details (id),
                                       FOREIGN KEY (aircraft_id) REFERENCES aircraft (id),

                                       PRIMARY KEY (id)
);
