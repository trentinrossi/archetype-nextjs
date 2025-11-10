'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { CardCreateRequest } from '@/types/card';
import { Input, Select, Button } from '@/components/ui';

/**
 * Create New Card Screen
 * Allows creation of a new credit card
 * 
 * Business Rules:
 * - Card number must be 16 digits
 * - Account ID must be 11 digits
 * - CVV code must be 3 digits
 * - Embossed name can only contain alphabets and spaces
 * - Active status must be Y or N
 * - Expiration date must be in the future
 */
export default function CreateCardPage() {
  const router = useRouter();
  
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  
  // Form data
  const [cardNumber, setCardNumber] = useState('');
  const [accountId, setAccountId] = useState('');
  const [embossedName, setEmbossedName] = useState('');
  const [expirationDate, setExpirationDate] = useState('');
  const [activeStatus, setActiveStatus] = useState('Y');
  const [cvvCode, setCvvCode] = useState('');
  
  // Validation errors
  const [cardNumberError, setCardNumberError] = useState('');
  const [accountIdError, setAccountIdError] = useState('');
  const [embossedNameError, setEmbossedNameError] = useState('');
  const [expirationDateError, setExpirationDateError] = useState('');
  const [cvvCodeError, setCvvCodeError] = useState('');

  /**
   * Validate card number
   * Must be 16 digits
   */
  const validateCardNumber = (value: string): boolean => {
    if (!value) {
      setCardNumberError('Card number is required');
      return false;
    }
    
    if (!/^\d{16}$/.test(value)) {
      setCardNumberError('Card number must be exactly 16 digits');
      return false;
    }
    
    setCardNumberError('');
    return true;
  };

  /**
   * Validate account ID
   * Must be 11 digits
   */
  const validateAccountId = (value: string): boolean => {
    if (!value) {
      setAccountIdError('Account ID is required');
      return false;
    }
    
    if (!/^\d{11}$/.test(value)) {
      setAccountIdError('Account ID must be exactly 11 digits');
      return false;
    }
    
    setAccountIdError('');
    return true;
  };

  /**
   * Validate embossed name
   * Must contain only alphabets and spaces
   */
  const validateEmbossedName = (value: string): boolean => {
    if (!value.trim()) {
      setEmbossedNameError('Embossed name is required');
      return false;
    }
    
    if (!/^[A-Za-z\s]+$/.test(value)) {
      setEmbossedNameError('Embossed name can only contain alphabets and spaces');
      return false;
    }
    
    setEmbossedNameError('');
    return true;
  };

  /**
   * Validate expiration date
   * Must be in the future
   */
  const validateExpirationDate = (value: string): boolean => {
    if (!value) {
      setExpirationDateError('Expiration date is required');
      return false;
    }
    
    const expDate = new Date(value);
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    
    if (expDate <= today) {
      setExpirationDateError('Expiration date must be in the future');
      return false;
    }
    
    setExpirationDateError('');
    return true;
  };

  /**
   * Validate CVV code
   * Must be 3 digits
   */
  const validateCvvCode = (value: string): boolean => {
    if (!value) {
      setCvvCodeError('CVV code is required');
      return false;
    }
    
    if (!/^\d{3}$/.test(value)) {
      setCvvCodeError('CVV code must be exactly 3 digits');
      return false;
    }
    
    setCvvCodeError('');
    return true;
  };

  /**
   * Handle card number change
   */
  const handleCardNumberChange = (value: string) => {
    setCardNumber(value);
    validateCardNumber(value);
  };

  /**
   * Handle account ID change
   */
  const handleAccountIdChange = (value: string) => {
    setAccountId(value);
    validateAccountId(value);
  };

  /**
   * Handle embossed name change
   */
  const handleEmbossedNameChange = (value: string) => {
    setEmbossedName(value);
    validateEmbossedName(value);
  };

  /**
   * Handle expiration date change
   */
  const handleExpirationDateChange = (value: string) => {
    setExpirationDate(value);
    validateExpirationDate(value);
  };

  /**
   * Handle CVV code change
   */
  const handleCvvCodeChange = (value: string) => {
    setCvvCode(value);
    validateCvvCode(value);
  };

  /**
   * Handle form submission
   */
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    // Validate all fields
    const isCardNumberValid = validateCardNumber(cardNumber);
    const isAccountIdValid = validateAccountId(accountId);
    const isEmbossedNameValid = validateEmbossedName(embossedName);
    const isExpirationDateValid = validateExpirationDate(expirationDate);
    const isCvvCodeValid = validateCvvCode(cvvCode);
    
    if (
      !isCardNumberValid ||
      !isAccountIdValid ||
      !isEmbossedNameValid ||
      !isExpirationDateValid ||
      !isCvvCodeValid
    ) {
      setError('Please correct the validation errors before submitting');
      return;
    }
    
    try {
      setSaving(true);
      setError(null);
      
      const createData: CardCreateRequest = {
        cardNumber: cardNumber,
        accountId: accountId,
        embossedName: embossedName.trim(),
        expirationDate: expirationDate,
        activeStatus: activeStatus,
        cvvCode: cvvCode,
      };
      
      await cardService.createCard(createData);
      
      // Navigate to card list
      router.push('/cards');
    } catch (err) {
      setError('Failed to create card. Please check if the card number already exists.');
      console.error('Failed to create card:', err);
    } finally {
      setSaving(false);
    }
  };

  /**
   * Handle cancel
   */
  const handleCancel = () => {
    router.push('/cards');
  };

  return (
    <div className="p-6 max-w-3xl mx-auto">
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-gray-900">Create New Card</h1>
        <p className="text-sm text-gray-600 mt-1">Add a new credit card to the system</p>
      </div>

      {/* Error Message */}
      {error && (
        <div className="mb-4 bg-red-50 border border-red-300 text-red-800 px-4 py-3 rounded">
          {error}
        </div>
      )}

      {/* Form */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6">
        <div className="space-y-4">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Card Information</h3>
          
          <Input
            label="Card Number"
            value={cardNumber}
            onChange={(e) => handleCardNumberChange(e.target.value)}
            error={cardNumberError}
            required
            maxLength={16}
            placeholder="Enter 16-digit card number"
            helperText="Must be exactly 16 digits"
          />
          
          <Input
            label="Account ID"
            value={accountId}
            onChange={(e) => handleAccountIdChange(e.target.value)}
            error={accountIdError}
            required
            maxLength={11}
            placeholder="Enter 11-digit account ID"
            helperText="Must be exactly 11 digits"
          />
          
          <Input
            label="Embossed Name"
            value={embossedName}
            onChange={(e) => handleEmbossedNameChange(e.target.value)}
            error={embossedNameError}
            required
            placeholder="Enter name as it appears on card"
            helperText="Only alphabets and spaces allowed"
          />
          
          <Input
            label="CVV Code"
            value={cvvCode}
            onChange={(e) => handleCvvCodeChange(e.target.value)}
            error={cvvCodeError}
            required
            maxLength={3}
            placeholder="Enter 3-digit CVV code"
            helperText="Must be exactly 3 digits"
            type="password"
          />
          
          <Input
            label="Expiration Date"
            type="date"
            value={expirationDate}
            onChange={(e) => handleExpirationDateChange(e.target.value)}
            error={expirationDateError}
            required
            helperText="Must be a future date"
          />
          
          <Select
            label="Active Status"
            value={activeStatus}
            onChange={(e) => setActiveStatus(e.target.value)}
            options={[
              { value: 'Y', label: 'Active (Y)' },
              { value: 'N', label: 'Inactive (N)' },
            ]}
            required
            helperText="Set initial status for the card"
          />
        </div>

        {/* Action Buttons */}
        <div className="mt-6 pt-6 border-t border-gray-200 flex gap-3">
          <Button type="submit" disabled={saving}>
            {saving ? 'Creating Card...' : 'Create Card'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={handleCancel}
            disabled={saving}
          >
            Cancel
          </Button>
        </div>
      </form>

      {/* Validation Rules */}
      <div className="mt-6 p-4 bg-gray-50 rounded-lg border border-gray-200">
        <h3 className="text-sm font-semibold text-gray-900 mb-2">Validation Rules:</h3>
        <ul className="text-sm text-gray-700 space-y-1">
          <li>• <strong>Card Number:</strong> Required, must be exactly 16 digits</li>
          <li>• <strong>Account ID:</strong> Required, must be exactly 11 digits</li>
          <li>• <strong>Embossed Name:</strong> Required, only alphabets and spaces allowed</li>
          <li>• <strong>CVV Code:</strong> Required, must be exactly 3 digits</li>
          <li>• <strong>Expiration Date:</strong> Required, must be in the future</li>
          <li>• <strong>Active Status:</strong> Must be Y (Active) or N (Inactive)</li>
        </ul>
      </div>

      {/* Instructions */}
      <div className="mt-4 p-4 bg-blue-50 rounded-lg border border-blue-200">
        <h3 className="text-sm font-semibold text-blue-900 mb-2">Instructions:</h3>
        <ul className="text-sm text-blue-800 space-y-1">
          <li>• Fill in all required fields with valid data</li>
          <li>• Ensure the card number is unique and not already in the system</li>
          <li>• Ensure the account ID exists in the system</li>
          <li>• Click <strong>Create Card</strong> to add the new card</li>
          <li>• Click <strong>Cancel</strong> to discard and return to card list</li>
        </ul>
      </div>
    </div>
  );
}
