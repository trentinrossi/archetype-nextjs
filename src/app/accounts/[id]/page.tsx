'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { customerService } from '@/services/customerService';
import { Account, Customer } from '@/types/account';
import { Button } from '@/components/ui';

export default function AccountViewPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [customer, setCustomer] = useState<Customer | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [infoMessage, setInfoMessage] = useState<string | null>(null);

  const fetchData = useCallback(async () => {
    if (!params.id) return;

    try {
      setLoading(true);
      setError(null);
      setInfoMessage(null);

      const data = await accountService.getAccountWithCustomer(params.id as string);
      
      setAccount(data.account);
      setCustomer(data.customer);
      setInfoMessage('Account and customer data retrieved successfully');
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to load account data';
      setError(errorMessage);
      console.error('Error fetching data:', err);
    } finally {
      setLoading(false);
    }
  }, [params.id]);

  useEffect(() => {
    fetchData();
  }, [fetchData]);

  const handleEdit = () => {
    if (account) {
      router.push(`/accounts/${account.accountId}/edit`);
    }
  };

  const handleBack = () => {
    router.push('/accounts');
  };

  if (loading) {
    return (
      <div className="p-6">
        <div className="text-center">Loading account data...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-6 max-w-6xl mx-auto">
        <div className="mb-6">
          <h1 className="text-2xl font-bold">Account View</h1>
        </div>
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
        <Button variant="secondary" onClick={handleBack}>
          Back to Accounts
        </Button>
      </div>
    );
  }

  if (!account || !customer) {
    return (
      <div className="p-6 max-w-6xl mx-auto">
        <div className="mb-6">
          <h1 className="text-2xl font-bold">Account View</h1>
        </div>
        <div className="bg-yellow-50 border border-yellow-200 text-yellow-700 px-4 py-3 rounded mb-4">
          Account or customer data not found
        </div>
        <Button variant="secondary" onClick={handleBack}>
          Back to Accounts
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-6xl mx-auto">
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Account View</h1>
        <p className="text-gray-600 mt-1">Detailed account and customer information</p>
      </div>

      {infoMessage && (
        <div className="bg-blue-50 border border-blue-200 text-blue-700 px-4 py-3 rounded mb-4">
          {infoMessage}
        </div>
      )}

      <div className="space-y-6">
        <div className="bg-white shadow rounded-lg p-6">
          <div className="flex justify-between items-center mb-4">
            <h2 className="text-xl font-semibold">Account Details</h2>
            <div className="flex gap-2">
              <Button onClick={handleEdit}>
                Edit Account
              </Button>
              <Button variant="secondary" onClick={handleBack}>
                Back to List
              </Button>
            </div>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">Account ID</label>
              <p className="mt-1 text-gray-900">{account.accountId}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Account Status</label>
              <p className="mt-1">
                <span className={`px-2 py-1 rounded text-xs font-semibold ${
                  account.activeStatus === 'Y' 
                    ? 'bg-green-100 text-green-800' 
                    : 'bg-gray-100 text-gray-800'
                }`}>
                  {account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                </span>
              </p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Current Balance</label>
              <p className="mt-1 text-gray-900">${account.currentBalance.toFixed(2)}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Credit Limit</label>
              <p className="mt-1 text-gray-900">${account.creditLimit.toFixed(2)}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Cash Credit Limit</label>
              <p className="mt-1 text-gray-900">${account.cashCreditLimit.toFixed(2)}</p>
            </div>

            {account.availableCredit !== undefined && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Available Credit</label>
                <p className="mt-1 text-gray-900">${account.availableCredit.toFixed(2)}</p>
              </div>
            )}

            {account.creditUtilization !== undefined && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Credit Utilization</label>
                <p className="mt-1 text-gray-900">{account.creditUtilization.toFixed(2)}%</p>
              </div>
            )}

            <div>
              <label className="block text-sm font-semibold text-gray-700">Current Cycle Credit</label>
              <p className="mt-1 text-gray-900">${account.currentCycleCredit.toFixed(2)}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Current Cycle Debit</label>
              <p className="mt-1 text-gray-900">${account.currentCycleDebit.toFixed(2)}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Open Date</label>
              <p className="mt-1 text-gray-900">{new Date(account.openDate).toLocaleDateString()}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Expiration Date</label>
              <p className="mt-1 text-gray-900">{new Date(account.expirationDate).toLocaleDateString()}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Reissue Date</label>
              <p className="mt-1 text-gray-900">{new Date(account.reissueDate).toLocaleDateString()}</p>
            </div>

            {account.groupId && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Group ID</label>
                <p className="mt-1 text-gray-900">{account.groupId}</p>
              </div>
            )}

            {account.daysUntilExpiration !== undefined && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Days Until Expiration</label>
                <p className="mt-1 text-gray-900">{account.daysUntilExpiration} days</p>
              </div>
            )}

            {account.accountAge !== undefined && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Account Age</label>
                <p className="mt-1 text-gray-900">{account.accountAge} days</p>
              </div>
            )}

            {account.isOverLimit !== undefined && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Over Limit</label>
                <p className="mt-1">
                  <span className={`px-2 py-1 rounded text-xs font-semibold ${
                    account.isOverLimit 
                      ? 'bg-red-100 text-red-800' 
                      : 'bg-green-100 text-green-800'
                  }`}>
                    {account.isOverLimit ? 'Yes' : 'No'}
                  </span>
                </p>
              </div>
            )}
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4">Customer Information</h2>
          
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">Customer ID</label>
              <p className="mt-1 text-gray-900">{customer.customerId}</p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Full Name</label>
              <p className="mt-1 text-gray-900">
                {customer.firstName} {customer.middleName ? `${customer.middleName} ` : ''}{customer.lastName}
              </p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">SSN</label>
              <p className="mt-1 text-gray-900 font-mono">
                {customerService.maskSSN(customer.ssn)}
              </p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Date of Birth</label>
              <p className="mt-1 text-gray-900">{new Date(customer.dateOfBirth).toLocaleDateString()}</p>
            </div>

            {customer.age !== undefined && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Age</label>
                <p className="mt-1 text-gray-900">{customer.age} years</p>
              </div>
            )}

            <div>
              <label className="block text-sm font-semibold text-gray-700">FICO Score</label>
              <p className="mt-1">
                <span className={`px-2 py-1 rounded text-xs font-semibold ${
                  customer.ficoScore >= 740 ? 'bg-green-100 text-green-800' :
                  customer.ficoScore >= 670 ? 'bg-blue-100 text-blue-800' :
                  customer.ficoScore >= 580 ? 'bg-yellow-100 text-yellow-800' :
                  'bg-red-100 text-red-800'
                }`}>
                  {customer.ficoScore}
                </span>
              </p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Primary Cardholder</label>
              <p className="mt-1">
                <span className={`px-2 py-1 rounded text-xs font-semibold ${
                  customer.primaryCardholderIndicator === 'Y' 
                    ? 'bg-blue-100 text-blue-800' 
                    : 'bg-gray-100 text-gray-800'
                }`}>
                  {customer.primaryCardholderIndicator === 'Y' ? 'Yes' : 'No'}
                </span>
              </p>
            </div>

            <div className="md:col-span-2">
              <label className="block text-sm font-semibold text-gray-700">Address</label>
              <p className="mt-1 text-gray-900">
                {customer.addressLine1}
                {customer.addressLine2 && <><br />{customer.addressLine2}</>}
                <br />
                {customer.addressLine3}, {customer.stateCode} {customer.zipCode}
                <br />
                {customer.countryCode}
              </p>
            </div>

            <div>
              <label className="block text-sm font-semibold text-gray-700">Primary Phone</label>
              <p className="mt-1 text-gray-900">{customer.phoneNumber1}</p>
            </div>

            {customer.phoneNumber2 && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Secondary Phone</label>
                <p className="mt-1 text-gray-900">{customer.phoneNumber2}</p>
              </div>
            )}

            {customer.governmentIssuedId && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">Government ID</label>
                <p className="mt-1 text-gray-900">{customer.governmentIssuedId}</p>
              </div>
            )}

            {customer.eftAccountId && (
              <div>
                <label className="block text-sm font-semibold text-gray-700">EFT Account ID</label>
                <p className="mt-1 text-gray-900">{customer.eftAccountId}</p>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
