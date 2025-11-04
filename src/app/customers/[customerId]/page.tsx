'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Customer } from '@/types/cardDemo';
import { customerService } from '@/services/customerService';

export default function CustomerDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [customer, setCustomer] = useState<Customer | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.customerId) {
      fetchCustomer(params.customerId as string);
    }
  }, [params.customerId]);

  const fetchCustomer = async (customerId: string) => {
    try {
      setLoading(true);
      setError(null);
      const data = await customerService.getCustomerById(customerId);
      setCustomer(data);
    } catch (err) {
      setError('Failed to load customer');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this customer?')) return;

    try {
      await customerService.deleteCustomer(params.customerId as string);
      router.push('/customers');
    } catch (err) {
      alert('Failed to delete customer');
      console.error(err);
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-gray-600">Loading customer details...</p>
        </div>
      </div>
    );
  }

  if (error || !customer) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <p className="text-red-800 font-medium">{error || 'Customer not found'}</p>
          <button
            onClick={() => router.push('/customers')}
            className="mt-4 px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700"
          >
            Back to Customers
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Customer Details</h1>
          <p className="text-gray-600 mt-1">Customer ID: {customer.customerId}</p>
        </div>
        <div className="flex gap-2">
          <button
            onClick={() => router.push(`/customers/${customer.customerId}/edit`)}
            className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
          >
            Edit Customer
          </button>
          <button
            onClick={handleDelete}
            className="px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700"
          >
            Delete Customer
          </button>
          <button
            onClick={() => router.push('/customers')}
            className="px-4 py-2 bg-gray-200 text-gray-700 rounded hover:bg-gray-300"
          >
            Back to List
          </button>
        </div>
      </div>

      <div className="space-y-6">
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Personal Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Customer ID</label>
              <p className="text-gray-900">{customer.customerId}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">First Name</label>
              <p className="text-gray-900">{customer.firstName}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Middle Name</label>
              <p className="text-gray-900">{customer.middleName || 'N/A'}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Last Name</label>
              <p className="text-gray-900">{customer.lastName}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Date of Birth</label>
              <p className="text-gray-900">{customer.dateOfBirth}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">SSN</label>
              <p className="text-gray-900">{customer.ssn.replace(/(\d{3})(\d{2})(\d{4})/, '$1-$2-$3')}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Government Issued ID</label>
              <p className="text-gray-900">{customer.governmentIssuedId || 'N/A'}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">FICO Credit Score</label>
              <p className="text-gray-900 font-semibold text-lg">{customer.ficoCreditScore}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Primary Cardholder</label>
              <p className="text-gray-900">{customer.primaryCardholderIndicator === 'Y' ? 'Yes' : 'No'}</p>
            </div>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Contact Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Phone Number 1</label>
              <p className="text-gray-900">{customer.phoneNumber1}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Phone Number 2</label>
              <p className="text-gray-900">{customer.phoneNumber2 || 'N/A'}</p>
            </div>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Address Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Address Line 1</label>
              <p className="text-gray-900">{customer.addressLine1}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Address Line 2</label>
              <p className="text-gray-900">{customer.addressLine2 || 'N/A'}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Address Line 3</label>
              <p className="text-gray-900">{customer.addressLine3 || 'N/A'}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">State</label>
              <p className="text-gray-900">{customer.stateCode}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Country</label>
              <p className="text-gray-900">{customer.countryCode}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">ZIP Code</label>
              <p className="text-gray-900">{customer.zipCode}</p>
            </div>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Banking Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">EFT Account ID</label>
              <p className="text-gray-900">{customer.eftAccountId || 'N/A'}</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
