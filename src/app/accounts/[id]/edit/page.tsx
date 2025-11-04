'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { accountService } from '@/services/accountService';
import { UpdateAccountData } from '@/types/account';

const EditAccountPage: React.FC = () => {
  const params = useParams();
  const router = useRouter();
  const accountId = params.id as string;
  
  const [loading, setLoading] = useState(true);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [formData, setFormData] = useState<UpdateAccountData>({
    activeStatus: 'Y',
    currentBalance: 0,
    creditLimit: 0,
    cashCreditLimit: 0,
    expirationDate: '',
    reissueDate: '',
    currentCycleCredit: 0,
    currentCycleDebit: 0,
    addressZipCode: '',
    groupId: '',
  });

  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    const fetchAccount = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await accountService.getAccountById(accountId);
        
        // Populate form with existing data
        setFormData({
          activeStatus: data.activeStatus,
          currentBalance: data.currentBalance,
          creditLimit: data.creditLimit,
          cashCreditLimit: data.cashCreditLimit,
          expirationDate: data.expirationDate.split('T')[0],
          reissueDate: data.reissueDate ? data.reissueDate.split('T')[0] : '',
          currentCycleCredit: data.currentCycleCredit,
          currentCycleDebit: data.currentCycleDebit,
          addressZipCode: data.addressZipCode,
          groupId: data.groupId,
        });
      } catch (err) {
        console.error('Error fetching account:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch account');
      } finally {
        setLoading(false);
      }
    };

    if (accountId) {
      fetchAccount();
    }
  }, [accountId]);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement>) => {
    const { name, value } = e.target;
    
    // Clear field error when user starts typing
    if (fieldErrors[name]) {
      setFieldErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[name];
        return newErrors;
      });
    }

    // Handle number fields
    if (['currentBalance', 'creditLimit', 'cashCreditLimit', 'currentCycleCredit', 'currentCycleDebit'].includes(name)) {
      setFormData(prev => ({
        ...prev,
        [name]: value === '' ? 0 : parseFloat(value),
      }));
    } else {
      setFormData(prev => ({
        ...prev,
        [name]: value,
      }));
    }
  };

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!formData.groupId || !formData.groupId.trim()) {
      errors.groupId = 'Group ID is required';
    }

    if (!formData.addressZipCode || !formData.addressZipCode.trim()) {
      errors.addressZipCode = 'Zip Code is required';
    }

    if (formData.creditLimit && formData.creditLimit < 0) {
      errors.creditLimit = 'Credit limit cannot be negative';
    }

    if (formData.cashCreditLimit && formData.cashCreditLimit < 0) {
      errors.cashCreditLimit = 'Cash credit limit cannot be negative';
    }

    if (formData.cashCreditLimit && formData.creditLimit && formData.cashCreditLimit > formData.creditLimit) {
      errors.cashCreditLimit = 'Cash credit limit cannot exceed credit limit';
    }

    if (!formData.expirationDate) {
      errors.expirationDate = 'Expiration date is required';
    }

    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      setError('Please fix the errors in the form');
      return;
    }

    try {
      setSubmitting(true);
      setError(null);

      await accountService.updateAccount(accountId, formData);
      
      // Redirect to account details on success
      router.push(`/accounts/${accountId}`);
    } catch (err) {
      console.error('Error updating account:', err);
      setError(err instanceof Error ? err.message : 'Failed to update account');
      setSubmitting(false);
    }
  };

  const handleCancel = () => {
    router.push(`/accounts/${accountId}`);
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading account...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Edit Account</h1>
        <p className="text-gray-600">Update account information for account {accountId}</p>
      </div>

      <div className="bg-white shadow-md rounded-lg overflow-hidden">
        <form onSubmit={handleSubmit} className="p-6">
          {error && (
            <div className="mb-6 bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded-md">
              <div className="flex">
                <div className="flex-shrink-0">
                  <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                    <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                  </svg>
                </div>
                <div className="ml-3">
                  <p className="text-sm font-medium">{error}</p>
                </div>
              </div>
            </div>
          )}

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {/* Active Status */}
            <div>
              <label htmlFor="activeStatus" className="block text-sm font-medium text-gray-700 mb-1">
                Status <span className="text-red-500">*</span>
              </label>
              <select
                id="activeStatus"
                name="activeStatus"
                value={formData.activeStatus}
                onChange={handleInputChange}
                className="flex h-10 w-full rounded-md border border-gray-300 bg-white px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:cursor-not-allowed disabled:opacity-50"
                disabled={submitting}
              >
                <option value="Y">Active</option>
                <option value="N">Inactive</option>
              </select>
            </div>

            {/* Group ID */}
            <div>
              <label htmlFor="groupId" className="block text-sm font-medium text-gray-700 mb-1">
                Group ID <span className="text-red-500">*</span>
              </label>
              <Input
                id="groupId"
                name="groupId"
                type="text"
                value={formData.groupId}
                onChange={handleInputChange}
                placeholder="Enter group ID"
                className={fieldErrors.groupId ? 'border-red-500' : ''}
                disabled={submitting}
              />
              {fieldErrors.groupId && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.groupId}</p>
              )}
            </div>

            {/* Current Balance */}
            <div>
              <label htmlFor="currentBalance" className="block text-sm font-medium text-gray-700 mb-1">
                Current Balance
              </label>
              <Input
                id="currentBalance"
                name="currentBalance"
                type="number"
                step="0.01"
                value={formData.currentBalance}
                onChange={handleInputChange}
                placeholder="0.00"
                disabled={submitting}
              />
            </div>

            {/* Credit Limit */}
            <div>
              <label htmlFor="creditLimit" className="block text-sm font-medium text-gray-700 mb-1">
                Credit Limit <span className="text-red-500">*</span>
              </label>
              <Input
                id="creditLimit"
                name="creditLimit"
                type="number"
                step="0.01"
                value={formData.creditLimit}
                onChange={handleInputChange}
                placeholder="0.00"
                className={fieldErrors.creditLimit ? 'border-red-500' : ''}
                disabled={submitting}
              />
              {fieldErrors.creditLimit && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.creditLimit}</p>
              )}
            </div>

            {/* Cash Credit Limit */}
            <div>
              <label htmlFor="cashCreditLimit" className="block text-sm font-medium text-gray-700 mb-1">
                Cash Credit Limit <span className="text-red-500">*</span>
              </label>
              <Input
                id="cashCreditLimit"
                name="cashCreditLimit"
                type="number"
                step="0.01"
                value={formData.cashCreditLimit}
                onChange={handleInputChange}
                placeholder="0.00"
                className={fieldErrors.cashCreditLimit ? 'border-red-500' : ''}
                disabled={submitting}
              />
              {fieldErrors.cashCreditLimit && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.cashCreditLimit}</p>
              )}
            </div>

            {/* Address Zip Code */}
            <div>
              <label htmlFor="addressZipCode" className="block text-sm font-medium text-gray-700 mb-1">
                Zip Code <span className="text-red-500">*</span>
              </label>
              <Input
                id="addressZipCode"
                name="addressZipCode"
                type="text"
                value={formData.addressZipCode}
                onChange={handleInputChange}
                placeholder="Enter zip code"
                className={fieldErrors.addressZipCode ? 'border-red-500' : ''}
                disabled={submitting}
              />
              {fieldErrors.addressZipCode && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.addressZipCode}</p>
              )}
            </div>

            {/* Expiration Date */}
            <div>
              <label htmlFor="expirationDate" className="block text-sm font-medium text-gray-700 mb-1">
                Expiration Date <span className="text-red-500">*</span>
              </label>
              <Input
                id="expirationDate"
                name="expirationDate"
                type="date"
                value={formData.expirationDate}
                onChange={handleInputChange}
                className={fieldErrors.expirationDate ? 'border-red-500' : ''}
                disabled={submitting}
              />
              {fieldErrors.expirationDate && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.expirationDate}</p>
              )}
            </div>

            {/* Reissue Date */}
            <div>
              <label htmlFor="reissueDate" className="block text-sm font-medium text-gray-700 mb-1">
                Reissue Date
              </label>
              <Input
                id="reissueDate"
                name="reissueDate"
                type="date"
                value={formData.reissueDate || ''}
                onChange={handleInputChange}
                disabled={submitting}
              />
            </div>

            {/* Current Cycle Credit */}
            <div>
              <label htmlFor="currentCycleCredit" className="block text-sm font-medium text-gray-700 mb-1">
                Current Cycle Credit
              </label>
              <Input
                id="currentCycleCredit"
                name="currentCycleCredit"
                type="number"
                step="0.01"
                value={formData.currentCycleCredit}
                onChange={handleInputChange}
                placeholder="0.00"
                disabled={submitting}
              />
            </div>

            {/* Current Cycle Debit */}
            <div>
              <label htmlFor="currentCycleDebit" className="block text-sm font-medium text-gray-700 mb-1">
                Current Cycle Debit
              </label>
              <Input
                id="currentCycleDebit"
                name="currentCycleDebit"
                type="number"
                step="0.01"
                value={formData.currentCycleDebit}
                onChange={handleInputChange}
                placeholder="0.00"
                disabled={submitting}
              />
            </div>
          </div>

          {/* Information Box */}
          <div className="mt-6 bg-blue-50 border border-blue-200 rounded-md p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-blue-400" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-blue-800">Update Information</h3>
                <div className="mt-2 text-sm text-blue-700">
                  <p>Note: The Account ID and Open Date cannot be changed after account creation.</p>
                </div>
              </div>
            </div>
          </div>

          {/* Form Actions */}
          <div className="mt-8 flex justify-end gap-3 pt-6 border-t border-gray-200">
            <Button
              type="button"
              variant="secondary"
              onClick={handleCancel}
              disabled={submitting}
            >
              Cancel
            </Button>
            <Button
              type="submit"
              disabled={submitting}
            >
              {submitting ? 'Updating...' : 'Update Account'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default EditAccountPage;
