'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { customerService } from '@/services/customerService';
import { Customer } from '@/types/cardServices';
import { Table, Button, Input, Select } from '@/components/ui';

export default function CustomersPage() {
  const router = useRouter();
  const [customers, setCustomers] = useState<Customer[]>([]);
  const [filteredCustomers, setFilteredCustomers] = useState<Customer[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchLastName, setSearchLastName] = useState('');
  const [filterState, setFilterState] = useState('');
  const [currentPage, setCurrentPage] = useState(1);
  const [itemsPerPage] = useState(10);

  const US_STATES = [
    { value: '', label: 'All States' },
    { value: 'AL', label: 'Alabama' },
    { value: 'CA', label: 'California' },
    { value: 'FL', label: 'Florida' },
    { value: 'GA', label: 'Georgia' },
    { value: 'IL', label: 'Illinois' },
    { value: 'NY', label: 'New York' },
    { value: 'TX', label: 'Texas' },
  ];

  useEffect(() => {
    fetchCustomers();
  }, []);

  useEffect(() => {
    applyFilters();
  }, [customers, searchLastName, filterState]);

  const fetchCustomers = async () => {
    try {
      setLoading(true);
      const data = await customerService.getCustomers();
      setCustomers(data);
      setError(null);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load customers');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const applyFilters = () => {
    let filtered = [...customers];

    if (searchLastName) {
      filtered = filtered.filter((customer) =>
        customer.lastName.toLowerCase().includes(searchLastName.toLowerCase())
      );
    }

    if (filterState) {
      filtered = filtered.filter((customer) => customer.stateCode === filterState);
    }

    setFilteredCustomers(filtered);
    setCurrentPage(1);
  };

  const handleDelete = async (customerId: string) => {
    if (!confirm('Are you sure you want to delete this customer?')) return;

    try {
      await customerService.deleteCustomer(customerId);
      fetchCustomers();
    } catch (err) {
      alert(err instanceof Error ? err.message : 'Failed to delete customer');
      console.error(err);
    }
  };

  const handleClearFilters = () => {
    setSearchLastName('');
    setFilterState('');
    setCurrentPage(1);
  };

  const indexOfLastItem = currentPage * itemsPerPage;
  const indexOfFirstItem = indexOfLastItem - itemsPerPage;
  const currentItems = filteredCustomers.slice(indexOfFirstItem, indexOfLastItem);
  const totalPages = Math.ceil(filteredCustomers.length / itemsPerPage);

  const handlePageChange = (pageNumber: number) => {
    setCurrentPage(pageNumber);
  };

  const formatPhoneNumber = (phone: string): string => {
    if (!phone) return '';
    const cleaned = phone.replace(/\D/g, '');
    if (cleaned.length === 10) {
      return `(${cleaned.slice(0, 3)}) ${cleaned.slice(3, 6)}-${cleaned.slice(6)}`;
    }
    return phone;
  };

  const getFicoScoreColor = (score: number): string => {
    if (score >= 800) return 'text-green-600 font-semibold';
    if (score >= 740) return 'text-blue-600 font-semibold';
    if (score >= 670) return 'text-yellow-600 font-semibold';
    if (score >= 580) return 'text-orange-600 font-semibold';
    return 'text-red-600 font-semibold';
  };

  const getFicoScoreBadge = (score: number): JSX.Element => {
    let bgColor = '';
    let textColor = '';
    let label = '';

    if (score >= 800) {
      bgColor = 'bg-green-100';
      textColor = 'text-green-800';
      label = 'Exceptional';
    } else if (score >= 740) {
      bgColor = 'bg-blue-100';
      textColor = 'text-blue-800';
      label = 'Very Good';
    } else if (score >= 670) {
      bgColor = 'bg-yellow-100';
      textColor = 'text-yellow-800';
      label = 'Good';
    } else if (score >= 580) {
      bgColor = 'bg-orange-100';
      textColor = 'text-orange-800';
      label = 'Fair';
    } else {
      bgColor = 'bg-red-100';
      textColor = 'text-red-800';
      label = 'Poor';
    }

    return (
      <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${bgColor} ${textColor}`}>
        {label}
      </span>
    );
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-16 w-16 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-gray-600">Loading customers...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex items-center">
            <span className="text-red-600 text-xl mr-3">⚠️</span>
            <div>
              <h3 className="text-red-800 font-semibold">Error Loading Customers</h3>
              <p className="text-red-600">{error}</p>
            </div>
          </div>
          <Button variant="secondary" onClick={fetchCustomers} className="mt-4">
            Retry
          </Button>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Customers</h1>
          <p className="text-sm text-gray-600 mt-1">
            Manage customer accounts and information
          </p>
        </div>
        <Button onClick={() => router.push('/customers/new')}>Create Customer</Button>
      </div>

      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-4">Search & Filter</h2>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <Input
            label="Search by Last Name"
            placeholder="Enter last name"
            value={searchLastName}
            onChange={(e) => setSearchLastName(e.target.value)}
          />

          <Select
            label="Filter by State"
            value={filterState}
            onChange={(e) => setFilterState(e.target.value)}
            options={US_STATES}
          />

          <div className="flex items-end">
            <Button
              variant="secondary"
              onClick={handleClearFilters}
              disabled={!searchLastName && !filterState}
            >
              Clear Filters
            </Button>
          </div>
        </div>

        <div className="mt-4 text-sm text-gray-600">
          Showing {currentItems.length} of {filteredCustomers.length} customers
          {(searchLastName || filterState) && ` (filtered from ${customers.length} total)`}
        </div>
      </div>

      {filteredCustomers.length === 0 ? (
        <div className="bg-white rounded-lg shadow p-6">
          <div className="text-center py-12">
            <span className="text-gray-400 text-5xl mb-4 block">👥</span>
            <h3 className="text-lg font-semibold text-gray-900 mb-2">No Customers Found</h3>
            <p className="text-gray-600">
              {searchLastName || filterState
                ? 'Try adjusting your search or filter criteria.'
                : 'Get started by creating your first customer.'}
            </p>
          </div>
        </div>
      ) : (
        <>
          <div className="bg-white rounded-lg shadow overflow-hidden">
            <Table
              columns={[
                {
                  key: 'customerId',
                  label: 'Customer ID',
                  render: (customer: Customer) => (
                    <span className="font-mono text-sm">{customer.customerId}</span>
                  ),
                },
                {
                  key: 'fullName',
                  label: 'Full Name',
                  render: (customer: Customer) => (
                    <div>
                      <div className="font-medium text-sm">{customer.fullName}</div>
                      {customer.primaryCardHolder && (
                        <span className="inline-flex items-center px-2 py-0.5 rounded text-xs font-medium bg-blue-100 text-blue-800 mt-1">
                          Primary
                        </span>
                      )}
                    </div>
                  ),
                },
                {
                  key: 'stateCode',
                  label: 'State',
                  render: (customer: Customer) => (
                    <span className="text-sm font-medium">{customer.stateCode}</span>
                  ),
                },
                {
                  key: 'zipCode',
                  label: 'ZIP Code',
                  render: (customer: Customer) => (
                    <span className="text-sm">{customer.zipCode}</span>
                  ),
                },
                {
                  key: 'phoneNumber1',
                  label: 'Phone Number',
                  render: (customer: Customer) => (
                    <span className="text-sm">{formatPhoneNumber(customer.phoneNumber1)}</span>
                  ),
                },
                {
                  key: 'ficoCreditScore',
                  label: 'FICO Score',
                  render: (customer: Customer) => (
                    <div className="flex items-center gap-2">
                      <span className={`text-sm ${getFicoScoreColor(customer.ficoCreditScore)}`}>
                        {customer.ficoCreditScore}
                      </span>
                      {getFicoScoreBadge(customer.ficoCreditScore)}
                    </div>
                  ),
                },
              ]}
              data={currentItems}
              onRowClick={(customer) => router.push(`/customers/${customer.customerId}`)}
              actions={(customer) => (
                <div className="flex gap-2">
                  <Button
                    size="sm"
                    onClick={(e) => {
                      e.stopPropagation();
                      router.push(`/customers/${customer.customerId}/edit`);
                    }}
                  >
                    Edit
                  </Button>
                  <Button
                    size="sm"
                    variant="danger"
                    onClick={(e) => {
                      e.stopPropagation();
                      handleDelete(customer.customerId);
                    }}
                  >
                    Delete
                  </Button>
                </div>
              )}
            />
          </div>

          {totalPages > 1 && (
            <div className="bg-white rounded-lg shadow mt-4 px-6 py-4">
              <div className="flex items-center justify-between">
                <div className="text-sm text-gray-700">
                  Page {currentPage} of {totalPages}
                </div>
                <div className="flex gap-2">
                  <Button
                    size="sm"
                    variant="secondary"
                    onClick={() => handlePageChange(currentPage - 1)}
                    disabled={currentPage === 1}
                  >
                    Previous
                  </Button>
                  {Array.from({ length: Math.min(totalPages, 5) }, (_, i) => {
                    let pageNum;
                    if (totalPages <= 5) {
                      pageNum = i + 1;
                    } else if (currentPage <= 3) {
                      pageNum = i + 1;
                    } else if (currentPage >= totalPages - 2) {
                      pageNum = totalPages - 4 + i;
                    } else {
                      pageNum = currentPage - 2 + i;
                    }
                    return (
                      <Button
                        key={pageNum}
                        size="sm"
                        variant={currentPage === pageNum ? 'primary' : 'secondary'}
                        onClick={() => handlePageChange(pageNum)}
                      >
                        {pageNum}
                      </Button>
                    );
                  })}
                  <Button
                    size="sm"
                    variant="secondary"
                    onClick={() => handlePageChange(currentPage + 1)}
                    disabled={currentPage === totalPages}
                  >
                    Next
                  </Button>
                </div>
              </div>
            </div>
          )}
        </>
      )}
    </div>
  );
}
