package aviatickets.app.util;


import aviatickets.app.actions.ActionInterface;
import aviatickets.app.customer.CustomerInterface;
import aviatickets.app.flight.FlightInterface;

import aviatickets.app.purchase.PurchaseInterface;
import aviatickets.app.util.entity.Actions;
import aviatickets.app.util.entity.Flights;
import aviatickets.app.util.entity.Purchases;
import aviatickets.app.util.entity.SignUps;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aot.hint.TypeReference;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.io.InputStream;

@RequiredArgsConstructor
@Component
public class JsonMockLoader implements CommandLineRunner {

	private final Logger log = LoggerFactory.getLogger(JsonMockLoader.class);
	private final ObjectMapper objectMapper;

	private final CustomerInterface customerService;
	private final FlightInterface flightService;
	private final ActionInterface actionService;
	private final PurchaseInterface purchaseService;

	@Override
	public void run(String... args) {

		try {
			this.addCustomerData();
			this.addFlightData();
			this.addPurchaseData();
			this.addActionData();

			log.info("Loading json files was complete");
		}  catch (Exception e) {
			log.info("Cannot load json mock file");
			log.error(e.getMessage());
		}


	}

	// ################################################################################################33

	private void addCustomerData() {

		try (InputStream customerStream = TypeReference.class.getResourceAsStream("/data/customers.json")) {

			SignUps customerList = this.objectMapper.readValue(customerStream, SignUps.class);
			log.info("customer size-> {} ", customerList.signUps().size());

			for (int i = 0; i < customerList.signUps().size(); i++) {
				log.info("Customer: {}", customerList.signUps().get(i));
				this.customerService.save(
					customerList.signUps().get(i).name(),
					customerList.signUps().get(i).password(),
					customerList.signUps().get(i).email()
				);
			}
		} catch (Exception e) {
			log.info(e.getMessage());
		}


	}

	private void addFlightData() {

		try (InputStream flightStream = TypeReference.class.getResourceAsStream("/data/flights.json")) {

			Flights flightsList = this.objectMapper.readValue(flightStream, Flights.class);
			log.info("flights size-> {} ", flightsList.flights().size());

			for (int i = 0; i < flightsList.flights().size(); i++) {
				log.info("Flight: {}", flightsList.flights().get(i));
				this.flightService.createFlight(flightsList.flights().get(i));
			}
		} catch (Exception e) {
			log.info(e.getMessage());
		}
	}

private void addActionData() {

	try (InputStream actionsStream = TypeReference.class.getResourceAsStream("/data/actions.json")) {

		Actions actionsList = this.objectMapper.readValue(actionsStream, Actions.class);
		log.info("actions size-> {} ", actionsList.actions().size());

			for (int i = 0; i < actionsList.actions().size(); i++) {
			log.info("Actions: {}", actionsList.actions().get(i));
			this.actionService.saveLog(actionsList.actions().get(i));
		}
	} catch (Exception e) {
		log.info(e.getMessage());
	}
}

private void addPurchaseData() {

	try (InputStream purchaseStream = TypeReference.class.getResourceAsStream("/data/purchases.json")) {

		Purchases purchaseList = this.objectMapper.readValue(purchaseStream, Purchases.class);
		log.info("Purchases size-> {} ", purchaseList.purchaseList().size());

		for (int i = 0; i < purchaseList.purchaseList().size(); i++) {
			log.info("Purchase: {}", purchaseList.purchaseList().get(i));
			this.purchaseService.create(purchaseList.purchaseList().get(i));
		}
	} catch (Exception e) {
		log.info(e.getMessage());
	}
}

}
