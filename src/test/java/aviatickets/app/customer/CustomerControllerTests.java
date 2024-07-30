//package aviatickets.app.customer;
//
//
//import aviatickets.app.actions.ActionService;
//import aviatickets.app.customer.entity.Customer;
//import aviatickets.app.databaseInit.DatabaseInit;
//import aviatickets.app.email.EmailService;
//import aviatickets.app.util.HelperService;
//import org.junit.jupiter.api.BeforeEach;
//import org.junit.jupiter.api.Test;
//import org.junit.jupiter.api.extension.ExtendWith;
//import org.junit.runner.RunWith;
//import org.mockito.Mock;
//
//import org.mockito.junit.jupiter.MockitoExtension;
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.boot.test.context.SpringBootTest;
//import org.springframework.test.context.junit4.SpringRunner;
//
//import java.sql.SQLException;
//
//import static org.mockito.Mockito.mock;
//
//@ExtendWith(MockitoExtension.class)
//public class CustomerControllerTests {
//
//  private static final Logger log = LoggerFactory.getLogger(CustomerControllerTests.class);
//
////	private DatabaseInit dbInit;
////	private HelperService helperService;
//
////	@Autowired
////	private CustomerService customerService;
////	private ActionService actionService;
////	private EmailService emailService;
////	CustomerRepository customerRepository;
////
//
//	@Mock
//	Customer customer;
//
//
//  @BeforeEach
//	public void init() {
//		log.info("Initializing CustomerControllerTests");
////		this.customer = mock(Customer.class);
//
////		this.dbInit = new DatabaseInit();
////		this.helperService = new HelperService();
////		this.emailService = new EmailService();
////		this.customerRepository = new CustomerRepository(this.dbInit, this.helperService);
////		this.customerService = new CustomerService(customerRepository, emailService, actionService);
//		log.info("CustomerControllerTests Initialized");
//	}
//
//	@Test
//	public void getSome() throws SQLException, ClassNotFoundException {
//		log.info("Customer name is -> {}", this.customer.name());
//	}
//
//}
