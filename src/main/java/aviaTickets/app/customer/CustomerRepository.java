package aviaTickets.app.customer;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

import aviaTickets.app.customer.entity.Role;
import aviaTickets.app.exception.PermissionDeniedException;
import aviaTickets.app.customer.dto.ChangePwdDto;
import aviaTickets.app.customer.entity.Customer;


// CustomerInteraction -> describe the main User interaction logic
abstract class CustomerInteraction {

  // check if user exists 
  abstract Boolean isCustomerExists(String email);
  // create user
  abstract void createCustomer(String name, String password, String email);
  // get user data by id 
  abstract Optional<Customer> getCustomer(Integer id);
  // get user data by email
  abstract Optional<Customer> getCustomer(String email);
  // get user list 
  abstract List<Customer> getAll();
  // update user data by <id> key with dto as second argument 
  abstract void updateProfile(Customer c, Integer id);
  // handle forgot password route and send new password to current user email
  abstract void changePassword(ChangePwdDto dto);
  // delete user by id
  abstract void deleteCustomer(Integer idToDelete, Integer customerId);
}

@Repository
public class CustomerRepository extends CustomerInteraction {
  
  private static final Logger log = LoggerFactory.getLogger(CustomerRepository.class);
  private final JdbcClient jdbcClient;

  public CustomerRepository(JdbcClient jdbcClient) {
    this.jdbcClient = jdbcClient;
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
    this.save(customer);
  }


  public Optional<Customer> getCustomer(Integer id) {
    return this.findById(id);
  }
  
  public Optional<Customer> getCustomer(String email) {
    return this.findByEmail(email);
  }
  
  public List<Customer> getAll() {
    return this.findAll();
  }

  public void updateProfile(Customer c , Integer id) {
    this.update(c, id);
  }

  public void changePassword(ChangePwdDto dto) {

  }

  public void deleteCustomer(Integer idToDelete, Integer customerId) {
    Boolean hasAccess = validatePermission(customerId);
    if(!hasAccess) throw new PermissionDeniedException();
    this.delete(idToDelete);
  }


  // ### ----------------------------------------------------------------------------------- ###

  private void save(Customer customer) {

    String userBase = "INSERT INTO customer (name, email, password) VALUES (?,?,?)"; 
    String userParams = "INSERT INTO customer_details (created_at, updated_at, is_activated, role, customer_id) VALUES (?,?,?,?,?)";
    // base user data 

    var updated = jdbcClient.sql(userBase)
      .params(List.of(customer.name(), customer.email(), customer.password()))
      .update();

    Assert.state(updated == 1, "Failed to create user " + customer.name());

    // get saved user 
    Optional<Customer> savedCusromer = this.findByEmail(customer.email());

    // user params data
    updated += jdbcClient.sql(userParams) 
      .params(List.of(customer.createdAt(), customer.updatedAt(), false, customer.role(), savedCusromer.get().id()))
      .update();

    Assert.state(updated == 2, "Failed to create user " + customer.name());
  }

  private List<Customer> findAll() {
    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
      + "FROM customer " 
      + "INNER JOIN customer_details ON customer.id = customer_details.customer_id ";

    return jdbcClient.sql(sqlStr)
      .query(Customer.class)
      .list();
  }


  private Optional<Customer> findById(Integer id) {

    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
      + "FROM customer " 
      + "INNER JOIN customer_details ON customer.id = customer_details.customer_id "
      + "WHERE customer.id=?";

    return jdbcClient.sql(sqlStr)
      .param(id)
      .query(Customer.class)
      .optional();
  }

  
  private Optional<Customer> findByEmail(String email) {

    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
      + "FROM customer " 
      + "INNER JOIN customer_details ON customer.id = customer_details.customer_id "
      + "WHERE customer.email=?";

    return jdbcClient.sql(sqlStr)
      .param(email)
      .query(Customer.class)
      .optional();
  }

  private void update(Customer u, Integer id) {

  }

  private void delete(Integer id) {

  }

  // validatePermission -> validate user permission by id
  private Boolean validatePermission(Integer id) {

    String sqlString = "SELECT customer_details.role"
      + "FROM customer_details "
      + "WHERE customer_id=?";

    Optional<Role> c = jdbcClient.sql(sqlString)
      .query(Role.class)
      .optional();

    if(c.get() != Role.ADMIN) {
      return false;
    } else {
      return true;
    }
  }

}
