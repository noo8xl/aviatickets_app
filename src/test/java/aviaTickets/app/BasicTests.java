//package aviaTickets.app;
//
//import aviatickets.app.Application;
//import aviatickets.app.customer.CustomerInterface;
//import aviatickets.app.customer.CustomerService;
//import aviatickets.app.util.HelperInterface;
//import aviatickets.app.util.HelperService;
//import aviatickets.app.util.JsonMockLoader;
//import org.junit.jupiter.api.*;
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.context.annotation.Bean;
//
//import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
//
//
//@SpringBootTest(classes = Application.class)
//class BasicTests {
//
//
//	private final Logger log = LoggerFactory.getLogger(BasicTests.class);
//
//	@Bean
//	CustomerService customerService;
//	@Bean
//	HelperService helper;
//
////	@Autowired
////	void setTitleRepository(
////			CustomerService customerService, HelperService helper) {
////		this.customerService = customerService;
////		this.helper = helper;
////	}
//
////	@BeforeEach
////	void setUp() {
//////		customerService = new CustomerService();
////		helper = new HelperService();
////	}
//
//	@Test
//	void testOne() {
//		log.info("testOne is OK.");
//	}
//
//	@Test
//	void testTwo() {
//		assertThat(this.customerService).isNotNull();
//		log.info("testTwo is OK.");
//	}
//
//	@Test
//	void testThree() {
//		assertThat(this.helper).isNotNull();
//		log.info("testThree is OK.");
//	}
//
//
//}
