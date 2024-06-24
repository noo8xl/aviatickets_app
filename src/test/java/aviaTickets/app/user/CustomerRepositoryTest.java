package aviaTickets.app.user;

import static org.junit.jupiter.api.Assertions.assertEquals;

// import java.sql.Connection;
// import java.util.Optional;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
// import org.junit.jupiter.params.ParameterizedTest;
// import org.junit.jupiter.params.provider.CsvSource;
// import org.junit.jupiter.params.provider.ValueSource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.simple.JdbcClient;

import aviaTickets.app.customer.CustomerRepository;
import aviaTickets.app.customer.entity.Customer;

public class CustomerRepositoryTest {

  private static final Logger log = LoggerFactory.getLogger(CustomerRepository.class);
  
  
  private CustomerRepository customerRepository;
  @Autowired
  JdbcClient jdbcClient;

  @BeforeEach
  void setUp() throws Exception {
    // Connection c = 
    // this.jdbcClient = jdbcClient;
    this.customerRepository = new CustomerRepository(this.jdbcClient);
    log.info("\nrepo -------------> ", customerRepository);
  }

  // @ParameterizedTest
  // @CsvSource({"test, pwd123test, example@email.com"})
  // @Test
  // void testCreateCustomer(String name, String password, String email) {
  //   customerRepository.createCustomer(name, password, email);
  // }

  // @Test
  // void testDeleteCustomer() {

  // }

  @Test
  void testGetAll() {
    List<Customer> customers = customerRepository.getAll();
    assertEquals(2, customers.size(), "Should have return 2 customers");
  }

  // @ParameterizedTest
  // @ValueSource(ints = {1})
  // @Test
  // void testGetCustomer(Integer id) {
  //   Optional<Customer> customer = customerRepository.getCustomer(1);
  //   assertEquals(customer, customer);
  // }

  // @Test
  // void testGetCustomer2() {

  // }

  // @Test
  // void testUpdateProfile() {

  // }
}
