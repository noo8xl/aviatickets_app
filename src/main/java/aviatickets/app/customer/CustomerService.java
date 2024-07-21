package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.Date;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import aviatickets.app.customer.entity.Customer;
import aviatickets.app.customer.entity.Role;
import aviatickets.app.exception.PermissionDeniedException;
import aviatickets.app.exception.ServerErrorException;

@Service
public class CustomerService implements CustomerInteraction {

  private static final Logger log = LoggerFactory.getLogger(CustomerService.class);
  private final CustomerRepository customerRepository;

  public CustomerService(CustomerRepository customerRepository) {
    this.customerRepository = customerRepository;
  }

  public Boolean isCustomerExists(String email) {
    Optional<Customer> c = getCustomer(email);
    return c.isEmpty();
  }

  public void createCustomer(String name, String password, String email) {
    // create activation link
    // set 2fa params
    Customer customer = new Customer(
        null,
        name,
        email,
        password,
        new Date(),
        new Date(),
        false,
        Role.USER);

    log.info("customer -> {}", customer);
    customerRepository.save(customer);
  }

  @Cacheable(key = "#id", value = "customer")
  public Optional<Customer> getCustomer(Integer id) {
    return customerRepository.findById(id);
  }

  public Optional<Customer> getCustomer(String email) {
    return customerRepository.findByEmail(email);
  }

  public List<Customer> getAll() {
    return customerRepository.findAll();
  }

  public void updateProfile(Integer id, Customer c) {
    customerRepository.update(c, id);
  }

  public Integer changePassword(String email, String pwd) throws ServerErrorException {
    Optional<Customer> c = getCustomer(email);

    if (c.isPresent()) {
      Customer updated = new Customer(null, c.get().name(), email, pwd, c.get().createdAt(), new Date(),
          c.get().isBanned(), c.get().role());
      customerRepository.update(updated, c.get().id());
      return c.get().id();
    }
    throw new ServerErrorException();
  }

  public void deleteCustomer(Integer idToDelete, Integer customerId) throws SQLException, ClassNotFoundException {
    customerRepository.delete(customerId, idToDelete);
  }

	public Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
		return customerRepository.getTwoStepStatus(email);
	}
}
