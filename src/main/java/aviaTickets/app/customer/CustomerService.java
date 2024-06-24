package aviaTickets.app.customer;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import aviaTickets.app.customer.dto.ChangePwdDto;
import aviaTickets.app.customer.entity.Customer;
import aviaTickets.app.customer.entity.Role;
import aviaTickets.app.exception.PermissionDeniedException;

@Service
public class CustomerService implements CustomerInteraction{
  
  private static final Logger log = LoggerFactory.getLogger(CustomerRepository.class);
  private final CustomerRepository customerRepository;

  public CustomerService(CustomerRepository customerRepository) {
    this.customerRepository = customerRepository;
  }

  public Boolean isCustomerExists(String email) {
    Optional<Customer> c = getCustomer(email);
    if(c.isEmpty()) return false;
    return true;
  }
  
  public void createCustomer(String name, String password, String email) {
    // create activation link
    // set 2fa params 
    Customer customer = new Customer(
      null, 
      name, 
      email, 
      password, 
      LocalDateTime.now(), 
      LocalDateTime.now(),
      Role.USER
    );

    log.info("customer -> " + customer);
    customerRepository.save(customer);
  }


  public Optional<Customer> getCustomer(Integer id) {
    return customerRepository.findById(id);
  }
  
  public Optional<Customer> getCustomer(String email) {
    return customerRepository.findByEmail(email);
  }
  
  public List<Customer> getAll() {
    return customerRepository.findAll();
  }

  public void updateProfile(Customer c , Integer id) {
    customerRepository.update(c, id);
  }

  public void changePassword(ChangePwdDto dto) {

  }

  public void deleteCustomer(Integer idToDelete, Integer customerId) {
    Boolean hasAccess = customerRepository.validatePermission(customerId);
    if(!hasAccess) throw new PermissionDeniedException();
    customerRepository.delete(idToDelete);
  }
}
