import { Customer, CreateCustomerRequest, UpdateCustomerRequest } from '@/types/customer';

const API_BASE_URL = '/api';

class CustomerService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCustomers(page: number = 0, size: number = 20, sort?: string): Promise<{ content: Customer[]; totalElements: number; totalPages: number }> {
    let url = `${API_BASE_URL}/customers?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to fetch customers');
    }
    return response.json();
  }

  async getCustomerById(id: string | number): Promise<Customer> {
    const response = await fetch(`${API_BASE_URL}/customers/${id}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      if (response.status === 404) {
        throw new Error('Customer not found');
      }
      throw new Error('Failed to fetch customer');
    }
    return response.json();
  }

  async createCustomer(data: CreateCustomerRequest): Promise<Customer> {
    const response = await fetch(`${API_BASE_URL}/customers`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to create customer');
    }
    return response.json();
  }

  async updateCustomer(id: string | number, data: UpdateCustomerRequest): Promise<Customer> {
    const response = await fetch(`${API_BASE_URL}/customers/${id}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(errorData.error || 'Failed to update customer');
    }
    return response.json();
  }

  async deleteCustomer(id: string | number): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/customers/${id}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    if (!response.ok) {
      throw new Error('Failed to delete customer');
    }
  }
}

export const customerService = new CustomerService();
