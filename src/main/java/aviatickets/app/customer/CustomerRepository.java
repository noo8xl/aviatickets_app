package aviatickets.app.customer;

import java.util.List;
import java.util.Optional;

import org.springframework.cache.annotation.Cacheable;
import org.springframework.jdbc.core.simple.JdbcClient;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

import aviatickets.app.customer.entity.Role;
import aviatickets.app.customer.entity.Customer;

@Repository
public class CustomerRepository {

  private final JdbcClient jdbcClient;

  public CustomerRepository(JdbcClient jdbcClient) {
    this.jdbcClient = jdbcClient;
  }

  public void save(Customer customer) {

    String userBase = "INSERT INTO customer (name, email, password) VALUES (?,?,?)";
    String userParams = "INSERT INTO customer_details (created_at, updated_at, is_banned, role, customer_id) VALUES (?,?,?,?,?)";
    // base user data

    var updated = jdbcClient.sql(userBase)
        .params(List.of(customer.name(), customer.email(), customer.password()))
        .update();

    Assert.state(updated == 1, "Failed to create user " + customer.name());

    // get saved user
    Optional<Customer> savedCusromer = this.findByEmail(customer.email());
    var customerId = 0;
    if (savedCusromer.isPresent())
      customerId = savedCusromer.get().id();
    // user params data
    updated += jdbcClient.sql(userParams)
        .params(List.of(customer.createdAt(), customer.updatedAt(), customer.isBanned(), customer.role(),
            customerId))
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

  @Cacheable("userList")
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

  public void update(Customer c, Integer id) {
    String updateBase = "UPDATE customer SET name=?, email=?, password=? WHERE id=?";
    String updateParams = "UPDATE customer_details SET updated_at=?, is_banned=?, role=? WHERE customer_id=?";

    var updated = jdbcClient.sql(updateBase)
        .params(List.of(c.name(), c.email(), c.password(), id))
        .update();

    Assert.state(updated == 1, "Failed to update customer " + c.name());

    updated += jdbcClient.sql(updateParams)
        .params(List.of(c.updatedAt(), c.isBanned(), c.role(), c.id()))
        .update();

    Assert.state(updated == 2, "Failed to update customer params " + c.name());

  }

  public void delete(Integer id) {
    // delete each record in each table by userId *
  }

  // validatePermission -> validate user permission by id
  public Boolean validatePermission(Integer id) {

    var r = Role.USER;
    String sqlString = "SELECT customer_details.role"
        + "FROM customer_details "
        + "WHERE customer_id=?";

    Optional<Role> c = jdbcClient.sql(sqlString)
        .param(id)
        .query(Role.class)
        .optional();

    if (c.isPresent())
      r = c.get();

    return r == Role.ADMIN;

  }

}
