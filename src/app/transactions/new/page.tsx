'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { transactionService } from '@/services/transactionService';
import { CreateTransactionData } from '@/types/account';

const NewTransactionPage: React.FC = () => {
  const router = useRouter();
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [formData, setFormData] = useState<CreateTransactionData>({
    cardNumber: '',
    accountId: '',
    transactionTypeCode: '',
    transactionCategoryCode: '',
    transactionSource: 'POS',
    transactionDescription: '',
    transactionAmount: 0,
    merchantId: 0,
    merchantName: '',
    merchantCity: '',
    merchantZip: '',
  });

  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>) => {
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
    if (['transactionAmount', 'merchantId'].includes(name)) {
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

    // Card Number validation
    if (!formData.cardNumber.trim()) {
      errors.cardNumber = 'Card number is required';
    }

    // Account ID validation
    if (!formData.accountId.trim()) {
      errors.accountId = 'Account ID is required';
    }

    // Transaction Type Code validation
    if (!formData.transactionTypeCode.trim()) {
      errors.transactionTypeCode = 'Transaction type code is required';
    }

    // Transaction Category Code validation
    if (!formData.transactionCategoryCode.trim()) {
      errors.transactionCategoryCode = 'Transaction category code is required';
    }

    // Transaction Source validation
    if (!formData.transactionSource.trim()) {
      errors.transactionSource = 'Transaction source is required';
    }

    // Transaction Description validation
    if (!formData.transactionDescription.trim()) {
      errors.transactionDescription = 'Transaction description is required';
    }

    // Transaction Amount validation
    if (formData.transactionAmount === 0) {
      errors.transactionAmount = 'Transaction amount cannot be zero';
    }

    // Merchant Name validation
    if (!formData.merchantName.trim()) {
      errors.merchantName = 'Merchant name is required';
    }

    // Merchant City validation
    if (!formData.merchantCity.trim()) {
      errors.merchantCity = 'Merchant city is required';
    }

    // Merchant Zip validation
    if (!formData.merchantZip.trim()) {
      errors.merchantZip = 'Merchant zip code is required';
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

      await transactionService.createTransaction(formData);
      
      // Redirect to transactions list on success
      router.push('/transactions');
    } catch (err) {
      console.error('Error creating transaction:', err);
      setError(err instanceof Error ? err.message : 'Failed to create transaction');
      setSubmitting(false);
    }
  };

  const handleCancel = () => {
    router.push('/transactions');
  };

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Create New Transaction</h1>
        <p className="text-gray-600">Record a new transaction for a card and account</p>
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

          {/* Card and Account Information Section */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Card & Account Information
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* Card Number */}
              <div>
                <label htmlFor="cardNumber" className="block text-sm font-medium text-gray-700 mb-1">
                  Card Number <span className="text-red-500">*</span>
                </label>
                <Input
                  id="cardNumber"
                  name="cardNumber"
                  type="text"
                  value={formData.cardNumber}
                  onChange={handleInputChange}
                  placeholder="Enter card number"
                  className={fieldErrors.cardNumber ? 'border-red-500 font-mono' : 'font-mono'}
                  disabled={submitting}
                />
                {fieldErrors.cardNumber && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.cardNumber}</p>
                )}
              </div>

              {/* Account ID */}
              <div>
                <label htmlFor="accountId" className="block text-sm font-medium text-gray-700 mb-1">
                  Account ID <span className="text-red-500">*</span>
                </label>
                <Input
                  id="accountId"
                  name="accountId"
                  type="text"
                  value={formData.accountId}
                  onChange={handleInputChange}
                  placeholder="Enter account ID"
                  className={fieldErrors.accountId ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.accountId && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.accountId}</p>
                )}
              </div>
            </div>
          </div>

          {/* Transaction Details Section */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Transaction Details
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* Transaction Type Code */}
              <div>
                <label htmlFor="transactionTypeCode" className="block text-sm font-medium text-gray-700 mb-1">
                  Transaction Type Code <span className="text-red-500">*</span>
                </label>
                <Input
                  id="transactionTypeCode"
                  name="transactionTypeCode"
                  type="text"
                  value={formData.transactionTypeCode}
                  onChange={handleInputChange}
                  placeholder="e.g., PUR, REF, PAY"
                  className={fieldErrors.transactionTypeCode ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.transactionTypeCode && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.transactionTypeCode}</p>
                )}
              </div>

              {/* Transaction Category Code */}
              <div>
                <label htmlFor="transactionCategoryCode" className="block text-sm font-medium text-gray-700 mb-1">
                  Transaction Category Code <span className="text-red-500">*</span>
                </label>
                <Input
                  id="transactionCategoryCode"
                  name="transactionCategoryCode"
                  type="text"
                  value={formData.transactionCategoryCode}
                  onChange={handleInputChange}
                  placeholder="e.g., RETAIL, FOOD, GAS"
                  className={fieldErrors.transactionCategoryCode ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.transactionCategoryCode && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.transactionCategoryCode}</p>
                )}
              </div>

              {/* Transaction Source */}
              <div>
                <label htmlFor="transactionSource" className="block text-sm font-medium text-gray-700 mb-1">
                  Transaction Source <span className="text-red-500">*</span>
                </label>
                <select
                  id="transactionSource"
                  name="transactionSource"
                  value={formData.transactionSource}
                  onChange={handleInputChange}
                  className="flex h-10 w-full rounded-md border border-gray-300 bg-white px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:cursor-not-allowed disabled:opacity-50"
                  disabled={submitting}
                >
                  <option value="POS">POS (Point of Sale)</option>
                  <option value="Online">Online</option>
                  <option value="ATM">ATM</option>
                  <option value="System">System</option>
                  <option value="Mobile">Mobile</option>
                </select>
              </div>

              {/* Transaction Amount */}
              <div>
                <label htmlFor="transactionAmount" className="block text-sm font-medium text-gray-700 mb-1">
                  Transaction Amount <span className="text-red-500">*</span>
                </label>
                <Input
                  id="transactionAmount"
                  name="transactionAmount"
                  type="number"
                  step="0.01"
                  value={formData.transactionAmount}
                  onChange={handleInputChange}
                  placeholder="0.00"
                  className={fieldErrors.transactionAmount ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.transactionAmount && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.transactionAmount}</p>
                )}
                <p className="mt-1 text-xs text-gray-500">Use negative values for refunds or credits</p>
              </div>

              {/* Transaction Description */}
              <div className="md:col-span-2">
                <label htmlFor="transactionDescription" className="block text-sm font-medium text-gray-700 mb-1">
                  Transaction Description <span className="text-red-500">*</span>
                </label>
                <textarea
                  id="transactionDescription"
                  name="transactionDescription"
                  value={formData.transactionDescription}
                  onChange={handleInputChange}
                  placeholder="Enter transaction description"
                  rows={3}
                  className={`flex w-full rounded-md border ${
                    fieldErrors.transactionDescription ? 'border-red-500' : 'border-gray-300'
                  } bg-white px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:cursor-not-allowed disabled:opacity-50`}
                  disabled={submitting}
                />
                {fieldErrors.transactionDescription && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.transactionDescription}</p>
                )}
              </div>
            </div>
          </div>

          {/* Merchant Information Section */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Merchant Information
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* Merchant ID */}
              <div>
                <label htmlFor="merchantId" className="block text-sm font-medium text-gray-700 mb-1">
                  Merchant ID
                </label>
                <Input
                  id="merchantId"
                  name="merchantId"
                  type="number"
                  value={formData.merchantId}
                  onChange={handleInputChange}
                  placeholder="0"
                  disabled={submitting}
                />
              </div>

              {/* Merchant Name */}
              <div>
                <label htmlFor="merchantName" className="block text-sm font-medium text-gray-700 mb-1">
                  Merchant Name <span className="text-red-500">*</span>
                </label>
                <Input
                  id="merchantName"
                  name="merchantName"
                  type="text"
                  value={formData.merchantName}
                  onChange={handleInputChange}
                  placeholder="Enter merchant name"
                  className={fieldErrors.merchantName ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.merchantName && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.merchantName}</p>
                )}
              </div>

              {/* Merchant City */}
              <div>
                <label htmlFor="merchantCity" className="block text-sm font-medium text-gray-700 mb-1">
                  Merchant City <span className="text-red-500">*</span>
                </label>
                <Input
                  id="merchantCity"
                  name="merchantCity"
                  type="text"
                  value={formData.merchantCity}
                  onChange={handleInputChange}
                  placeholder="Enter city"
                  className={fieldErrors.merchantCity ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.merchantCity && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.merchantCity}</p>
                )}
              </div>

              {/* Merchant Zip */}
              <div>
                <label htmlFor="merchantZip" className="block text-sm font-medium text-gray-700 mb-1">
                  Merchant Zip Code <span className="text-red-500">*</span>
                </label>
                <Input
                  id="merchantZip"
                  name="merchantZip"
                  type="text"
                  value={formData.merchantZip}
                  onChange={handleInputChange}
                  placeholder="Enter zip code"
                  className={fieldErrors.merchantZip ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.merchantZip && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.merchantZip}</p>
                )}
              </div>
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
                <h3 className="text-sm font-medium text-blue-800">Transaction Recording Notes</h3>
                <div className="mt-2 text-sm text-blue-700">
                  <ul className="list-disc list-inside space-y-1">
                    <li>The transaction will be recorded with the current timestamp</li>
                    <li>Ensure the card number and account ID are valid and match</li>
                    <li>Negative amounts represent refunds or credits</li>
                    <li>Transaction codes should follow your organization&apos;s standards</li>
                  </ul>
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
              {submitting ? 'Creating...' : 'Create Transaction'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default NewTransactionPage;
