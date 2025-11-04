'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import { customerService } from '@/services/customerService';
import { Customer } from '@/types/account';

const CustomerDetailPage: React.FC = () => {
  const params = useParams();
  const router = useRouter();
  const customerId = params.id as string;
  
  const [customer, setCustomer] = useState<Customer | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchCustomer = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await customerService.getCustomerById(customerId);
        setCustomer(data);
      } catch (err) {
        console.error('Error fetching customer:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch customer');
      } finally {
        setLoading(false);
      }
    };

    if (customerId) {
      fetchCustomer();
    }
  }, [customerId]);

  const formatDate = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'long',
      day: 'numeric',
    });
  };

  const formatPhoneNumber = (phone: string): string => {
    if (!phone) return 'N/A';
    const cleaned = phone.replace(/\D/g, '');
    if (cleaned.length === 10) {
      return `(${cleaned.slice(0, 3)}) ${cleaned.slice(3, 6)}-${cleaned.slice(6)}`;
    }
    return phone;
  };

  const maskSSN = (ssn: string): string => {
    if (!ssn) return 'N/A';
    const cleaned = ssn.replace(/\D/g, '');
    if (cleaned.length === 9) {
      return `***-**-${cleaned.slice(5)}`;
    }
    return '***-**-****';
  };

  const calculateAge = (dateOfBirth: string): number => {
    if (!dateOfBirth) return 0;
    const today = new Date();
    const birthDate = new Date(dateOfBirth);
    let age = today.getFullYear() - birthDate.getFullYear();
    const monthDiff = today.getMonth() - birthDate.getMonth();
    if (monthDiff < 0 || (monthDiff === 0 && today.getDate() < birthDate.getDate())) {
      age--;
    }
    return age;
  };

  const getCreditScoreColor = (score: number): string => {
    if (score >= 800) return 'text-green-600';
    if (score >= 740) return 'text-blue-600';
    if (score >= 670) return 'text-yellow-600';
    if (score >= 580) return 'text-orange-600';
    return 'text-red-600';
  };

  const getCreditScoreRating = (score: number): string => {
    if (score >= 800) return 'Excellent';
    if (score >= 740) return 'Very Good';
    if (score >= 670) return 'Good';
    if (score >= 580) return 'Fair';
    return 'Poor';
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading customer details...</p>
        </div>
      </div>
    );
  }

  if (error || !customer) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error || 'Customer not found'}</p>
          <Button onClick={() => router.push('/customers')}>Back to Customers</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      {/* Header */}
      <div className="mb-8">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-3xl font-bold text-gray-900 mb-2">
              {customer.firstName} {customer.middleName && `${customer.middleName} `}{customer.lastName}
            </h1>
            <p className="text-gray-600">Customer ID: {customer.customerId}</p>
          </div>
          <div className="flex gap-3">
            <Button variant="secondary" onClick={() => router.push('/customers')}>
              Back to List
            </Button>
            <Button onClick={() => router.push(`/customers/${customerId}/edit`)}>
              Edit Customer
            </Button>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Main Content */}
        <div className="lg:col-span-2 space-y-6">
          {/* Personal Information */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Personal Information</h2>
            </div>
            <div className="p-6 grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">First Name</label>
                <p className="text-base text-gray-900">{customer.firstName}</p>
              </div>
              {customer.middleName && (
                <div>
                  <label className="block text-sm font-medium text-gray-500 mb-1">Middle Name</label>
                  <p className="text-base text-gray-900">{customer.middleName}</p>
                </div>
              )}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Last Name</label>
                <p className="text-base text-gray-900">{customer.lastName}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Date of Birth</label>
                <p className="text-base text-gray-900">
                  {formatDate(customer.dateOfBirth)}
                  <span className="ml-2 text-sm text-gray-500">
                    (Age: {calculateAge(customer.dateOfBirth)})
                  </span>
                </p>
              </div>
            </div>
          </div>

          {/* Contact Information */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Contact Information</h2>
            </div>
            <div className="p-6 grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Primary Phone</label>
                <p className="text-base text-gray-900">{formatPhoneNumber(customer.phoneNumber1)}</p>
              </div>
              {customer.phoneNumber2 && (
                <div>
                  <label className="block text-sm font-medium text-gray-500 mb-1">Secondary Phone</label>
                  <p className="text-base text-gray-900">{formatPhoneNumber(customer.phoneNumber2)}</p>
                </div>
              )}
            </div>
          </div>

          {/* Address Information */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Address</h2>
            </div>
            <div className="p-6">
              <div className="space-y-2">
                <p className="text-base text-gray-900">{customer.addressLine1}</p>
                {customer.addressLine2 && (
                  <p className="text-base text-gray-900">{customer.addressLine2}</p>
                )}
                {customer.addressLine3 && (
                  <p className="text-base text-gray-900">{customer.addressLine3}</p>
                )}
                <p className="text-base text-gray-900">
                  {customer.stateCode}, {customer.countryCode} {customer.zipCode}
                </p>
              </div>
            </div>
          </div>

          {/* Identification */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Identification</h2>
            </div>
            <div className="p-6 grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Social Security Number</label>
                <p className="text-base text-gray-900 font-mono">{maskSSN(customer.ssn)}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Government Issued ID</label>
                <p className="text-base text-gray-900">{customer.governmentIssuedId || 'N/A'}</p>
              </div>
            </div>
          </div>
        </div>

        {/* Sidebar */}
        <div className="lg:col-span-1 space-y-6">
          {/* Credit Score Card */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Credit Score</h2>
            </div>
            <div className="p-6 text-center">
              <div className={`text-5xl font-bold mb-2 ${getCreditScoreColor(customer.ficoCreditScore)}`}>
                {customer.ficoCreditScore}
              </div>
              <div className="text-lg font-medium text-gray-700 mb-4">
                {getCreditScoreRating(customer.ficoCreditScore)}
              </div>
              <div className="w-full bg-gray-200 rounded-full h-3 mb-2">
                <div
                  className={`h-3 rounded-full transition-all ${
                    customer.ficoCreditScore >= 800 ? 'bg-green-500' :
                    customer.ficoCreditScore >= 740 ? 'bg-blue-500' :
                    customer.ficoCreditScore >= 670 ? 'bg-yellow-500' :
                    customer.ficoCreditScore >= 580 ? 'bg-orange-500' :
                    'bg-red-500'
                  }`}
                  style={{ width: `${(customer.ficoCreditScore / 850) * 100}%` }}
                />
              </div>
              <div className="text-xs text-gray-500">Score Range: 300 - 850</div>
            </div>
          </div>

          {/* Account Details */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Account Details</h2>
            </div>
            <div className="p-6 space-y-4">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">EFT Account ID</label>
                <p className="text-base text-gray-900">{customer.eftAccountId || 'N/A'}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Cardholder Status</label>
                <div className="mt-1">
                  {customer.primaryCardholderIndicator === 'Y' ? (
                    <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-blue-100 text-blue-800">
                      Primary Cardholder
                    </span>
                  ) : (
                    <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-gray-100 text-gray-800">
                      Secondary Cardholder
                    </span>
                  )}
                </div>
              </div>
            </div>
          </div>

          {/* Quick Actions */}
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <h3 className="text-sm font-medium text-blue-900 mb-3">Quick Actions</h3>
            <div className="space-y-2">
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/accounts?customer=${customerId}`)}
              >
                View Accounts
              </Button>
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/cards?customer=${customerId}`)}
              >
                View Cards
              </Button>
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/transactions?customer=${customerId}`)}
              >
                View Transactions
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default CustomerDetailPage;
