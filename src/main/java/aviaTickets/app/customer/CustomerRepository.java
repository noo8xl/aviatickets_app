package aviaTickets.app.customer;

import java.util.List;
import java.util.Optional;


// import org.slf4j.Logger;
// import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

import aviaTickets.app.customer.entity.Role;
import aviaTickets.app.customer.entity.Customer;

@Repository
public class CustomerRepository  {
  
  // private static final Logger log = LoggerFactory.getLogger(CustomerRepository.class);
  private final JdbcClient jdbcClient;

  public CustomerRepository(JdbcClient jdbcClient) {
    this.jdbcClient = jdbcClient;
  }

  
  public void save(Customer customer) {

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

  public List<Customer> findAll() {
    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
      + "FROM customer " 
      + "INNER JOIN customer_details ON customer.id = customer_details.customer_id ";

    return jdbcClient.sql(sqlStr)
      .query(Customer.class)
      .list();
  }


  public Optional<Customer> findById(Integer id) {

    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
      + "FROM customer " 
      + "INNER JOIN customer_details ON customer.id = customer_details.customer_id "
      + "WHERE customer.id=?";

    return jdbcClient.sql(sqlStr)
      .param(id)
      .query(Customer.class)
      .optional();
  }

  
  public Optional<Customer> findByEmail(String email) {

    String sqlStr = "SELECT customer.id, customer.name, customer.email, customer_details.created_at, customer_details.updated_at, customer_details.role "
      + "FROM customer " 
      + "INNER JOIN customer_details ON customer.id = customer_details.customer_id "
      + "WHERE customer.email=?";

    return jdbcClient.sql(sqlStr)
      .param(email)
      .query(Customer.class)
      .optional();
  }

  public void update(Customer u, Integer id) {

  }

  public void delete(Integer id) {

  }

  // validatePermission -> validate user permission by id
  public Boolean validatePermission(Integer id) {

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
