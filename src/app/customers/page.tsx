'use client';

import React, { useEffect, useState } from 'react';
import { Customer } from '@/types/account';
import { Table, Button, Input, Modal } from '@/components/ui';
import { useRouter } from 'next/navigation';
import { customerService } from '@/services/customerService';

const CustomersPage: React.FC = () => {
  const router = useRouter();
  const [customers, setCustomers] = useState<Customer[]>([]);
  const [filteredCustomers, setFilteredCustomers] = useState<Customer[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [deleteModalOpen, setDeleteModalOpen] = useState(false);
  const [customerToDelete, setCustomerToDelete] = useState<string | null>(null);
  const [deleting, setDeleting] = useState(false);

  const fetchCustomers = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await customerService.getCustomers();
      setCustomers(data);
      setFilteredCustomers(data);
    } catch (err) {
      console.error('Error fetching customers:', err);
      setError(err instanceof Error ? err.message : 'An unexpected error occurred');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchCustomers();
  }, []);

  useEffect(() => {
    if (searchTerm.trim() === '') {
      setFilteredCustomers(customers);
    } else {
      const term = searchTerm.toLowerCase();
      const filtered = customers.filter(customer =>
        customer.customerId.toLowerCase().includes(term) ||
        customer.firstName.toLowerCase().includes(term) ||
        customer.lastName.toLowerCase().includes(term) ||
        customer.ssn.includes(term)
      );
      setFilteredCustomers(filtered);
    }
  }, [searchTerm, customers]);

  const handleViewDetails = (customerId: string) => {
    router.push(`/customers/${customerId}`);
  };

  const handleEdit = (customerId: string) => {
    router.push(`/customers/${customerId}/edit`);
  };

  const handleDeleteClick = (customerId: string) => {
    setCustomerToDelete(customerId);
    setDeleteModalOpen(true);
  };

  const handleDeleteConfirm = async () => {
    if (!customerToDelete) return;

    try {
      setDeleting(true);
      await customerService.deleteCustomer(customerToDelete);
      setCustomers(prev => prev.filter(customer => customer.customerId !== customerToDelete));
      setDeleteModalOpen(false);
      setCustomerToDelete(null);
    } catch (err) {
      console.error('Error deleting customer:', err);
      setError(err instanceof Error ? err.message : 'Failed to delete customer');
    } finally {
      setDeleting(false);
    }
  };

  const handleDeleteCancel = () => {
    setDeleteModalOpen(false);
    setCustomerToDelete(null);
  };

  const handleCreateNew = () => {
    router.push('/customers/new');
  };

  const formatDate = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  const maskSSN = (ssn: string): string => {
    if (!ssn || ssn.length < 4) return '***-**-****';
    return `***-**-${ssn.slice(-4)}`;
  };

  const columns = [
    {
      key: 'customerId',
      header: 'Customer ID',
      render: (customer: Customer) => (
        <span className="font-medium text-gray-900">{customer.customerId}</span>
      ),
    },
    {
      key: 'name',
      header: 'Name',
      render: (customer: Customer) => (
        <div>
          <div className="font-medium text-gray-900">
            {customer.firstName} {customer.middleName} {customer.lastName}
          </div>
        </div>
      ),
    },
    {
      key: 'dateOfBirth',
      header: 'Date of Birth',
      render: (customer: Customer) => (
        <span className="text-gray-900">{formatDate(customer.dateOfBirth)}</span>
      ),
    },
    {
      key: 'ssn',
      header: 'SSN',
      render: (customer: Customer) => (
        <span className="text-gray-900 font-mono">{maskSSN(customer.ssn)}</span>
      ),
    },
    {
      key: 'address',
      header: 'Address',
      render: (customer: Customer) => (
        <div className="text-sm text-gray-900">
          <div>{customer.addressLine1}</div>
          {customer.addressLine2 && <div>{customer.addressLine2}</div>}
          <div>{customer.stateCode} {customer.zipCode}</div>
        </div>
      ),
    },
    {
      key: 'phoneNumber',
      header: 'Phone',
      render: (customer: Customer) => (
        <span className="text-gray-900">{customer.phoneNumber1}</span>
      ),
    },
    {
      key: 'ficoScore',
      header: 'FICO Score',
      render: (customer: Customer) => (
        <span className={`font-medium ${
          customer.ficoCreditScore >= 700 ? 'text-green-600' :
          customer.ficoCreditScore >= 600 ? 'text-yellow-600' :
          'text-red-600'
        }`}>
          {customer.ficoCreditScore || 'N/A'}
        </span>
      ),
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (customer: Customer) => (
        <div className="flex gap-2">
          <Button
            variant="secondary"
            size="sm"
            onClick={() => handleViewDetails(customer.customerId)}
          >
            View
          </Button>
          <Button
            variant="secondary"
            size="sm"
            onClick={() => handleEdit(customer.customerId)}
          >
            Edit
          </Button>
          <Button
            variant="danger"
            size="sm"
            onClick={() => handleDeleteClick(customer.customerId)}
          >
            Delete
          </Button>
        </div>
      ),
    },
  ];

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading customers...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error}</p>
          <Button onClick={fetchCustomers}>Retry</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Customers</h1>
        <p className="text-gray-600">Manage and view all customer information</p>
      </div>

      <div className="mb-6 flex flex-wrap gap-4 items-end">
        <div className="flex-1 min-w-[300px]">
          <label htmlFor="search" className="block text-sm font-medium text-gray-700 mb-1">
            Search Customers
          </label>
          <Input
            id="search"
            type="text"
            placeholder="Search by ID, name, or SSN..."
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
          />
        </div>

        <div className="flex-shrink-0">
          <Button onClick={handleCreateNew}>Create New Customer</Button>
        </div>
      </div>

      {filteredCustomers.length === 0 ? (
        <div className="text-center py-12 bg-gray-50 rounded-lg">
          <svg
            className="mx-auto h-12 w-12 text-gray-400"
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
            aria-hidden="true"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"
            />
          </svg>
          <h3 className="mt-2 text-sm font-medium text-gray-900">No customers found</h3>
          <p className="mt-1 text-sm text-gray-500">
            {customers.length === 0
              ? 'Get started by creating a new customer.'
              : 'Try adjusting your search to see more results.'}
          </p>
          {customers.length === 0 && (
            <div className="mt-6">
              <Button onClick={handleCreateNew}>Create New Customer</Button>
            </div>
          )}
        </div>
      ) : (
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <Table
            data={filteredCustomers}
            columns={columns}
            keyExtractor={(customer) => customer.customerId}
          />
          <div className="px-6 py-4 bg-gray-50 border-t border-gray-200">
            <p className="text-sm text-gray-700">
              Showing <span className="font-medium">{filteredCustomers.length}</span> of{' '}
              <span className="font-medium">{customers.length}</span> customers
            </p>
          </div>
        </div>
      )}

      <Modal
        isOpen={deleteModalOpen}
        onClose={handleDeleteCancel}
        title="Delete Customer"
      >
        <div className="mt-2">
          <p className="text-sm text-gray-500">
            Are you sure you want to delete customer <strong>{customerToDelete}</strong>? This action cannot be undone.
          </p>
        </div>
        <div className="mt-6 flex justify-end gap-3">
          <Button
            variant="secondary"
            onClick={handleDeleteCancel}
            disabled={deleting}
          >
            Cancel
          </Button>
          <Button
            variant="danger"
            onClick={handleDeleteConfirm}
            disabled={deleting}
          >
            {deleting ? 'Deleting...' : 'Delete'}
          </Button>
        </div>
      </Modal>
    </div>
  );
};

export default CustomersPage;
