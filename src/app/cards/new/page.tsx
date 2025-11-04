'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { cardService } from '@/services/cardService';
import { CreateCardData } from '@/types/account';

const NewCardPage: React.FC = () => {
  const router = useRouter();
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [formData, setFormData] = useState<CreateCardData>({
    cardNumber: '',
    accountId: '',
    cvvCode: '',
    embossedName: '',
    expirationDate: '',
    activeStatus: 'Y',
  });

  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});

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

    setFormData(prev => ({
      ...prev,
      [name]: value,
    }));
  };

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    // Card Number validation (16 digits)
    if (!formData.cardNumber.trim()) {
      errors.cardNumber = 'Card number is required';
    } else if (!/^\d{16}$/.test(formData.cardNumber.replace(/\s/g, ''))) {
      errors.cardNumber = 'Card number must be 16 digits';
    }

    // Account ID validation
    if (!formData.accountId.trim()) {
      errors.accountId = 'Account ID is required';
    }

    // CVV validation (3 or 4 digits)
    if (!formData.cvvCode.trim()) {
      errors.cvvCode = 'CVV code is required';
    } else if (!/^\d{3,4}$/.test(formData.cvvCode)) {
      errors.cvvCode = 'CVV must be 3 or 4 digits';
    }

    // Embossed Name validation
    if (!formData.embossedName.trim()) {
      errors.embossedName = 'Cardholder name is required';
    } else if (formData.embossedName.length > 26) {
      errors.embossedName = 'Name must be 26 characters or less';
    }

    // Expiration Date validation
    if (!formData.expirationDate) {
      errors.expirationDate = 'Expiration date is required';
    } else {
      const expDate = new Date(formData.expirationDate);
      const today = new Date();
      today.setHours(0, 0, 0, 0);
      
      if (expDate <= today) {
        errors.expirationDate = 'Expiration date must be in the future';
      }
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

      await cardService.createCard(formData);
      
      // Redirect to cards list on success
      router.push('/cards');
    } catch (err) {
      console.error('Error creating card:', err);
      setError(err instanceof Error ? err.message : 'Failed to create card');
      setSubmitting(false);
    }
  };

  const handleCancel = () => {
    router.push('/cards');
  };

  // Format card number with spaces for display
  const formatCardNumberDisplay = (value: string) => {
    const cleaned = value.replace(/\s/g, '');
    const match = cleaned.match(/.{1,4}/g);
    return match ? match.join(' ') : cleaned;
  };

  const handleCardNumberChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value.replace(/\s/g, '').replace(/\D/g, '');
    if (value.length <= 16) {
      handleInputChange({
        ...e,
        target: { ...e.target, name: 'cardNumber', value }
      });
    }
  };

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Create New Card</h1>
        <p className="text-gray-600">Fill in the details to issue a new credit card</p>
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
            {/* Card Number */}
            <div className="md:col-span-2">
              <label htmlFor="cardNumber" className="block text-sm font-medium text-gray-700 mb-1">
                Card Number <span className="text-red-500">*</span>
              </label>
              <Input
                id="cardNumber"
                name="cardNumber"
                type="text"
                value={formatCardNumberDisplay(formData.cardNumber)}
                onChange={handleCardNumberChange}
                placeholder="1234 5678 9012 3456"
                className={fieldErrors.cardNumber ? 'border-red-500 font-mono' : 'font-mono'}
                disabled={submitting}
                maxLength={19}
              />
              {fieldErrors.cardNumber && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.cardNumber}</p>
              )}
              <p className="mt-1 text-xs text-gray-500">Enter 16-digit card number</p>
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

            {/* Embossed Name */}
            <div className="md:col-span-2">
              <label htmlFor="embossedName" className="block text-sm font-medium text-gray-700 mb-1">
                Cardholder Name (Embossed) <span className="text-red-500">*</span>
              </label>
              <Input
                id="embossedName"
                name="embossedName"
                type="text"
                value={formData.embossedName}
                onChange={handleInputChange}
                placeholder="JOHN DOE"
                className={fieldErrors.embossedName ? 'border-red-500 uppercase' : 'uppercase'}
                disabled={submitting}
                maxLength={26}
              />
              {fieldErrors.embossedName && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.embossedName}</p>
              )}
              <p className="mt-1 text-xs text-gray-500">
                Name as it will appear on the card (max 26 characters)
              </p>
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

            {/* CVV Code */}
            <div>
              <label htmlFor="cvvCode" className="block text-sm font-medium text-gray-700 mb-1">
                CVV Code <span className="text-red-500">*</span>
              </label>
              <Input
                id="cvvCode"
                name="cvvCode"
                type="text"
                value={formData.cvvCode}
                onChange={handleInputChange}
                placeholder="123"
                className={fieldErrors.cvvCode ? 'border-red-500 font-mono' : 'font-mono'}
                disabled={submitting}
                maxLength={4}
              />
              {fieldErrors.cvvCode && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.cvvCode}</p>
              )}
              <p className="mt-1 text-xs text-gray-500">3 or 4 digit security code</p>
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
                <h3 className="text-sm font-medium text-blue-800">Card Issuance Information</h3>
                <div className="mt-2 text-sm text-blue-700">
                  <ul className="list-disc list-inside space-y-1">
                    <li>Ensure the account ID exists and is active</li>
                    <li>Card number must be unique and follow industry standards</li>
                    <li>CVV code will be securely stored and encrypted</li>
                    <li>Expiration date typically ranges from 3-5 years from today</li>
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
              {submitting ? 'Creating...' : 'Create Card'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default NewCardPage;
