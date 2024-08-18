package aviatickets.app.customer;

import aviatickets.app.actions.ActionService;
import aviatickets.app.notification.NotificationInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;


// references ->
// https://spring.academy/guides/spring-spring-boot-testing
// https://www.freecodecamp.org/news/java-unit-testing/

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@ExtendWith(MockitoExtension.class)
class CustomerControllerTests {

  private static final Logger log = LoggerFactory.getLogger(CustomerControllerTests.class);

	@Autowired
	private CustomerService customerService;
	@MockBean
	private ActionService actionService;
	@MockBean
	private NotificationInterface notificationService;
 	@Autowired
 	CustomerRepository customerRepository;


  @BeforeEach
	public void init() {
		log.info("CustomerControllerTests Initialized");
	}

	@Test
	public void getSome() throws Exception {
//		when(this.customerService.findOne(1)).thenReturn(new Customer());

		log.info("\nCustomer name is -> {}", this.customerService.findOne(1));
	}

}
