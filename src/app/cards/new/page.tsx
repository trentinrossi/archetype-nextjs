'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { CreateCardRequest } from '@/types/card';
import { Input, Select, Button } from '@/components/ui';

/**
 * Create Card Screen
 * 
 * Purpose: Create a new credit card
 * 
 * Features:
 * - Enter card number (16 digits)
 * - Enter account ID (11 digits)
 * - Enter embossed name (alphabets and spaces only)
 * - Enter expiration date (must be in future)
 * - Select active status (Y/N)
 * - Full validation as per business rules
 */
export default function CreateCardPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateCardRequest>({
    cardNumber: '',
    accountId: '',
    embossedName: '',
    expirationDate: '',
    activeStatus: 'Y',
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  /**
   * Validate card number (must be 16 digits)
   */
  const validateCardNumber = (value: string): boolean => {
    if (!value) {
      setValidationErrors((prev) => ({
        ...prev,
        cardNumber: 'Card number is required',
      }));
      return false;
    }
    
    if (!/^\d{16}$/.test(value)) {
      setValidationErrors((prev) => ({
        ...prev,
        cardNumber: 'Card number must be exactly 16 digits',
      }));
      return false;
    }
    
    setValidationErrors((prev) => {
      const { cardNumber, ...rest } = prev;
      return rest;
    });
    return true;
  };

  /**
   * Validate account ID (must be 11 digits)
   */
  const validateAccountId = (value: string): boolean => {
    if (!value) {
      setValidationErrors((prev) => ({
        ...prev,
        accountId: 'Account ID is required',
      }));
      return false;
    }
    
    if (!/^\d{11}$/.test(value)) {
      setValidationErrors((prev) => ({
        ...prev,
        accountId: 'Account ID must be exactly 11 digits',
      }));
      return false;
    }
    
    setValidationErrors((prev) => {
      const { accountId, ...rest } = prev;
      return rest;
    });
    return true;
  };

  /**
   * Validate embossed name (alphabets and spaces only)
   */
  const validateEmbossedName = (value: string): boolean => {
    if (!value || value.trim().length === 0) {
      setValidationErrors((prev) => ({
        ...prev,
        embossedName: 'Embossed name is required',
      }));
      return false;
    }
    
    if (!/^[A-Za-z\s]+$/.test(value)) {
      setValidationErrors((prev) => ({
        ...prev,
        embossedName: 'Embossed name can only contain alphabets and spaces',
      }));
      return false;
    }
    
    setValidationErrors((prev) => {
      const { embossedName, ...rest } = prev;
      return rest;
    });
    return true;
  };

  /**
   * Validate expiration date (must be in future)
   */
  const validateExpirationDate = (value: string): boolean => {
    if (!value) {
      setValidationErrors((prev) => ({
        ...prev,
        expirationDate: 'Expiration date is required',
      }));
      return false;
    }
    
    const expirationDate = new Date(value);
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    
    if (expirationDate <= today) {
      setValidationErrors((prev) => ({
        ...prev,
        expirationDate: 'Expiration date must be in the future',
      }));
      return false;
    }
    
    setValidationErrors((prev) => {
      const { expirationDate, ...rest } = prev;
      return rest;
    });
    return true;
  };

  /**
   * Validate active status (must be Y or N)
   */
  const validateActiveStatus = (value: string): boolean => {
    if (!value || (value !== 'Y' && value !== 'N')) {
      setValidationErrors((prev) => ({
        ...prev,
        activeStatus: 'Active status must be Y or N',
      }));
      return false;
    }
    
    setValidationErrors((prev) => {
      const { activeStatus, ...rest } = prev;
      return rest;
    });
    return true;
  };

  /**
   * Validate all fields
   */
  const validateForm = (): boolean => {
    const isCardNumberValid = validateCardNumber(formData.cardNumber);
    const isAccountIdValid = validateAccountId(formData.accountId);
    const isNameValid = validateEmbossedName(formData.embossedName);
    const isDateValid = validateExpirationDate(formData.expirationDate);
    const isStatusValid = validateActiveStatus(formData.activeStatus);
    
    return (
      isCardNumberValid &&
      isAccountIdValid &&
      isNameValid &&
      isDateValid &&
      isStatusValid
    );
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);

    // Validate form
    if (!validateForm()) {
      setError('Please correct the validation errors before submitting');
      return;
    }

    try {
      setLoading(true);
      await cardService.createCard(formData);
      router.push('/cards');
    } catch (err) {
      setError('Failed to create card. The card number may already exist.');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleCancel = () => {
    router.push('/cards');
  };

  return (
    <div className="p-6 max-w-4xl mx-auto">
      {/* Header */}
      <div className="bg-blue-900 text-white p-4 mb-6 rounded-lg">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">CREATE NEW CARD</h1>
            <p className="text-sm">Add a new credit card to the system</p>
          </div>
        </div>
      </div>

      {/* Page Title */}
      <div className="mb-6">
        <h2 className="text-xl font-bold">Card Information</h2>
        <p className="text-sm text-gray-600 mt-1">
          Enter the details for the new credit card
        </p>
      </div>

      {/* Error Message */}
      {error && (
        <div className="bg-red-100 text-red-800 p-4 rounded-lg mb-6">
          {error}
        </div>
      )}

      {/* Create Form */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6">
        <div className="space-y-6">
          {/* Card Number */}
          <div>
            <Input
              label="Card Number"
              value={formData.cardNumber}
              onChange={(e) => {
                setFormData({ ...formData, cardNumber: e.target.value });
                validateCardNumber(e.target.value);
              }}
              placeholder="Enter 16-digit card number"
              maxLength={16}
              required
            />
            {validationErrors.cardNumber && (
              <p className="text-sm text-red-600 mt-1">
                {validationErrors.cardNumber}
              </p>
            )}
            <p className="text-xs text-gray-500 mt-1">
              Must be exactly 16 digits
            </p>
          </div>

          {/* Account ID */}
          <div>
            <Input
              label="Account ID"
              value={formData.accountId}
              onChange={(e) => {
                setFormData({ ...formData, accountId: e.target.value });
                validateAccountId(e.target.value);
              }}
              placeholder="Enter 11-digit account ID"
              maxLength={11}
              required
            />
            {validationErrors.accountId && (
              <p className="text-sm text-red-600 mt-1">
                {validationErrors.accountId}
              </p>
            )}
            <p className="text-xs text-gray-500 mt-1">
              Must be exactly 11 digits
            </p>
          </div>

          {/* Embossed Name */}
          <div>
            <Input
              label="Embossed Name"
              value={formData.embossedName}
              onChange={(e) => {
                setFormData({ ...formData, embossedName: e.target.value });
                validateEmbossedName(e.target.value);
              }}
              placeholder="Enter name as it appears on card"
              required
            />
            {validationErrors.embossedName && (
              <p className="text-sm text-red-600 mt-1">
                {validationErrors.embossedName}
              </p>
            )}
            <p className="text-xs text-gray-500 mt-1">
              Only alphabets and spaces allowed
            </p>
          </div>

          {/* Expiration Date */}
          <div>
            <Input
              label="Expiration Date"
              type="date"
              value={formData.expirationDate}
              onChange={(e) => {
                setFormData({ ...formData, expirationDate: e.target.value });
                validateExpirationDate(e.target.value);
              }}
              required
            />
            {validationErrors.expirationDate && (
              <p className="text-sm text-red-600 mt-1">
                {validationErrors.expirationDate}
              </p>
            )}
            <p className="text-xs text-gray-500 mt-1">
              Must be a future date
            </p>
          </div>

          {/* Active Status */}
          <div>
            <Select
              label="Active Status"
              value={formData.activeStatus}
              onChange={(e) => {
                setFormData({ ...formData, activeStatus: e.target.value });
                validateActiveStatus(e.target.value);
              }}
              options={[
                { value: 'Y', label: 'Active (Y)' },
                { value: 'N', label: 'Inactive (N)' },
              ]}
              required
            />
            {validationErrors.activeStatus && (
              <p className="text-sm text-red-600 mt-1">
                {validationErrors.activeStatus}
              </p>
            )}
          </div>
        </div>

        {/* Form Actions */}
        <div className="mt-8 flex gap-2 pt-6 border-t">
          <Button
            type="submit"
            disabled={loading || Object.keys(validationErrors).length > 0}
          >
            {loading ? 'Creating...' : 'Create Card'}
          </Button>
          <Button type="button" variant="secondary" onClick={handleCancel}>
            Cancel
          </Button>
        </div>
      </form>

      {/* Help Text */}
      <div className="mt-6 bg-blue-50 p-4 rounded-lg">
        <h3 className="font-semibold text-blue-900 mb-2">Validation Rules</h3>
        <ul className="text-sm text-blue-800 space-y-1">
          <li>• Card number must be exactly 16 digits</li>
          <li>• Account ID must be exactly 11 digits</li>
          <li>• Embossed name can only contain alphabets and spaces</li>
          <li>• Expiration date must be in the future</li>
          <li>• Active status must be Y (Active) or N (Inactive)</li>
        </ul>
      </div>
    </div>
  );
}
