'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { CreateCardRequest } from '@/types/card';
import { Input, Select, Button } from '@/components/ui';

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
  const [errors, setErrors] = useState<{
    cardNumber?: string;
    accountId?: string;
    embossedName?: string;
    expirationDate?: string;
    activeStatus?: string;
  }>({});

  const validateForm = (): boolean => {
    const newErrors: {
      cardNumber?: string;
      accountId?: string;
      embossedName?: string;
      expirationDate?: string;
      activeStatus?: string;
    } = {};
    let isValid = true;

    // Validate card number - must be 16 digits
    if (!formData.cardNumber || formData.cardNumber.trim() === '') {
      newErrors.cardNumber = 'Card number is required';
      isValid = false;
    } else if (!/^\d{16}$/.test(formData.cardNumber.trim())) {
      newErrors.cardNumber = 'Card number must be exactly 16 digits';
      isValid = false;
    }

    // Validate account ID - must be 11 digits
    if (!formData.accountId || formData.accountId.trim() === '') {
      newErrors.accountId = 'Account ID is required';
      isValid = false;
    } else if (!/^\d{11}$/.test(formData.accountId.trim())) {
      newErrors.accountId = 'Account ID must be exactly 11 digits';
      isValid = false;
    }

    // Validate embossed name - only alphabets and spaces
    if (!formData.embossedName || formData.embossedName.trim() === '') {
      newErrors.embossedName = 'Embossed name is required';
      isValid = false;
    } else if (!/^[A-Za-z\s]+$/.test(formData.embossedName)) {
      newErrors.embossedName = 'Embossed name can only contain alphabets and spaces';
      isValid = false;
    }

    // Validate expiration date - must be in the future
    if (!formData.expirationDate) {
      newErrors.expirationDate = 'Expiration date is required';
      isValid = false;
    } else {
      const expDate = new Date(formData.expirationDate);
      const today = new Date();
      today.setHours(0, 0, 0, 0);
      
      if (expDate <= today) {
        newErrors.expirationDate = 'Expiration date must be in the future';
        isValid = false;
      }
    }

    // Validate active status - must be Y or N
    if (!formData.activeStatus) {
      newErrors.activeStatus = 'Active status is required';
      isValid = false;
    } else if (formData.activeStatus !== 'Y' && formData.activeStatus !== 'N') {
      newErrors.activeStatus = 'Active status must be Y or N';
      isValid = false;
    }

    setErrors(newErrors);
    return isValid;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!validateForm()) {
      return;
    }

    try {
      setLoading(true);
      const cleanedData: CreateCardRequest = {
        cardNumber: formData.cardNumber.trim(),
        accountId: formData.accountId.trim(),
        embossedName: formData.embossedName.trim(),
        expirationDate: formData.expirationDate,
        activeStatus: formData.activeStatus,
      };
      
      const newCard = await cardService.createCard(cleanedData);
      router.push(`/cards/${newCard.cardNumber}`);
    } catch (err) {
      alert('Failed to create card. Please check if the account exists and the card number is unique.');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleCancel = () => {
    if (confirm('Are you sure you want to cancel? All entered data will be lost.')) {
      router.push('/cards');
    }
  };

  const generateCardNumber = () => {
    const randomNumber = Math.floor(Math.random() * 10000000000000000).toString().padStart(16, '0');
    setFormData({ ...formData, cardNumber: randomNumber });
    setErrors({ ...errors, cardNumber: undefined });
  };

  const getDefaultExpirationDate = () => {
    const date = new Date();
    date.setFullYear(date.getFullYear() + 3);
    return date.toISOString().split('T')[0];
  };

  return (
    <div className="p-6 max-w-3xl">
      {/* Header Section */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Create New Card</h1>
        <p className="text-sm text-gray-600">Add a new credit card to the system</p>
      </div>

      {/* Form Section */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6 space-y-6">
        <div className="space-y-4">
          {/* Card Number */}
          <div>
            <div className="flex items-end gap-2">
              <div className="flex-1">
                <Input
                  label="Card Number"
                  value={formData.cardNumber}
                  onChange={(e) => {
                    setFormData({ ...formData, cardNumber: e.target.value });
                    setErrors({ ...errors, cardNumber: undefined });
                  }}
                  error={errors.cardNumber}
                  required
                  placeholder="Enter 16-digit card number"
                  helperText="Must be exactly 16 digits"
                  maxLength={16}
                />
              </div>
              <Button
                type="button"
                variant="secondary"
                onClick={generateCardNumber}
              >
                Generate
              </Button>
            </div>
          </div>

          {/* Account ID */}
          <Input
            label="Account ID"
            value={formData.accountId}
            onChange={(e) => {
              setFormData({ ...formData, accountId: e.target.value });
              setErrors({ ...errors, accountId: undefined });
            }}
            error={errors.accountId}
            required
            placeholder="Enter 11-digit account ID"
            helperText="Must be exactly 11 digits and must exist in the system"
            maxLength={11}
          />

          {/* Embossed Name */}
          <Input
            label="Embossed Name"
            value={formData.embossedName}
            onChange={(e) => {
              setFormData({ ...formData, embossedName: e.target.value });
              setErrors({ ...errors, embossedName: undefined });
            }}
            error={errors.embossedName}
            required
            placeholder="Enter name as it will appear on card"
            helperText="Only alphabets and spaces allowed"
          />

          {/* Expiration Date */}
          <div>
            <div className="flex items-end gap-2">
              <div className="flex-1">
                <Input
                  label="Expiration Date"
                  type="date"
                  value={formData.expirationDate}
                  onChange={(e) => {
                    setFormData({ ...formData, expirationDate: e.target.value });
                    setErrors({ ...errors, expirationDate: undefined });
                  }}
                  error={errors.expirationDate}
                  required
                  helperText="Must be a future date"
                />
              </div>
              <Button
                type="button"
                variant="secondary"
                onClick={() => {
                  setFormData({ ...formData, expirationDate: getDefaultExpirationDate() });
                  setErrors({ ...errors, expirationDate: undefined });
                }}
              >
                +3 Years
              </Button>
            </div>
          </div>

          {/* Active Status */}
          <Select
            label="Active Status"
            value={formData.activeStatus}
            onChange={(e) => {
              setFormData({ ...formData, activeStatus: e.target.value });
              setErrors({ ...errors, activeStatus: undefined });
            }}
            options={[
              { value: 'Y', label: 'Active (Y)' },
              { value: 'N', label: 'Inactive (N)' },
            ]}
            error={errors.activeStatus}
            required
          />
        </div>

        {/* Warning Messages */}
        {formData.activeStatus === 'N' && (
          <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ Warning</p>
            <p className="text-sm mt-1">
              You are creating an inactive card. It will not be usable for transactions until activated.
            </p>
          </div>
        )}

        {/* Action Buttons */}
        <div className="flex gap-2 pt-4 border-t">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Card'}
          </Button>
          <Button type="button" variant="secondary" onClick={handleCancel}>
            Cancel
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/cards')}
          >
            Back to List
          </Button>
        </div>
      </form>

      {/* Information Section */}
      <div className="mt-6 bg-blue-50 border border-blue-200 text-blue-800 px-4 py-3 rounded">
        <p className="font-semibold">ℹ️ Card Creation Guidelines</p>
        <ul className="text-sm mt-2 list-disc list-inside space-y-1">
          <li>Card number must be unique and exactly 16 digits</li>
          <li>Account ID must exist in the system and be exactly 11 digits</li>
          <li>Embossed name can only contain letters and spaces</li>
          <li>Expiration date must be a future date (typically 3-5 years from now)</li>
          <li>Active status determines if the card can be used immediately</li>
          <li>Use the "Generate" button to create a random card number</li>
          <li>Use the "+3 Years" button to set expiration date 3 years from today</li>
        </ul>
      </div>
    </div>
  );
}
