'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { customerService } from '@/services/customerService';
import { CreateCustomerData } from '@/types/account';

const NewCustomerPage: React.FC = () => {
  const router = useRouter();
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [formData, setFormData] = useState<CreateCustomerData>({
    customerId: '',
    firstName: '',
    middleName: '',
    lastName: '',
    addressLine1: '',
    addressLine2: '',
    addressLine3: '',
    stateCode: '',
    countryCode: 'US',
    zipCode: '',
    phoneNumber1: '',
    phoneNumber2: '',
    ssn: '',
    governmentIssuedId: '',
    dateOfBirth: '',
    eftAccountId: '',
    primaryCardholderIndicator: 'Y',
    ficoCreditScore: 0,
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

    // Handle number fields
    if (name === 'ficoCreditScore') {
      setFormData(prev => ({
        ...prev,
        [name]: value === '' ? 0 : parseInt(value, 10),
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

    // Required fields
    if (!formData.customerId.trim()) {
      errors.customerId = 'Customer ID is required';
    }

    if (!formData.firstName.trim()) {
      errors.firstName = 'First name is required';
    }

    if (!formData.lastName.trim()) {
      errors.lastName = 'Last name is required';
    }

    if (!formData.addressLine1.trim()) {
      errors.addressLine1 = 'Address line 1 is required';
    }

    if (!formData.stateCode.trim()) {
      errors.stateCode = 'State code is required';
    } else if (formData.stateCode.length !== 2) {
      errors.stateCode = 'State code must be 2 characters (e.g., CA, NY)';
    }

    if (!formData.countryCode.trim()) {
      errors.countryCode = 'Country code is required';
    } else if (formData.countryCode.length !== 2) {
      errors.countryCode = 'Country code must be 2 characters (e.g., US)';
    }

    if (!formData.zipCode.trim()) {
      errors.zipCode = 'Zip code is required';
    }

    if (!formData.phoneNumber1.trim()) {
      errors.phoneNumber1 = 'Primary phone number is required';
    }

    if (!formData.ssn.trim()) {
      errors.ssn = 'SSN is required';
    } else if (!/^\d{9}$/.test(formData.ssn.replace(/-/g, ''))) {
      errors.ssn = 'SSN must be 9 digits';
    }

    if (!formData.governmentIssuedId.trim()) {
      errors.governmentIssuedId = 'Government issued ID is required';
    }

    if (!formData.dateOfBirth) {
      errors.dateOfBirth = 'Date of birth is required';
    } else {
      const dob = new Date(formData.dateOfBirth);
      const today = new Date();
      const age = today.getFullYear() - dob.getFullYear();
      if (age < 18) {
        errors.dateOfBirth = 'Customer must be at least 18 years old';
      }
      if (dob > today) {
        errors.dateOfBirth = 'Date of birth cannot be in the future';
      }
    }

    if (!formData.eftAccountId.trim()) {
      errors.eftAccountId = 'EFT Account ID is required';
    }

    // FICO score validation
    if (formData.ficoCreditScore < 300 || formData.ficoCreditScore > 850) {
      errors.ficoCreditScore = 'FICO score must be between 300 and 850';
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

      await customerService.createCustomer(formData);
      
      // Redirect to customers list on success
      router.push('/customers');
    } catch (err) {
      console.error('Error creating customer:', err);
      setError(err instanceof Error ? err.message : 'Failed to create customer');
      setSubmitting(false);
    }
  };

  const handleCancel = () => {
    router.push('/customers');
  };

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Create New Customer</h1>
        <p className="text-gray-600">Fill in the details to create a new customer profile</p>
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

          {/* Personal Information Section */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Personal Information
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* Customer ID */}
              <div>
                <label htmlFor="customerId" className="block text-sm font-medium text-gray-700 mb-1">
                  Customer ID <span className="text-red-500">*</span>
                </label>
                <Input
                  id="customerId"
                  name="customerId"
                  type="text"
                  value={formData.customerId}
                  onChange={handleInputChange}
                  placeholder="Enter customer ID"
                  className={fieldErrors.customerId ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.customerId && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.customerId}</p>
                )}
              </div>

              {/* Primary Cardholder Indicator */}
              <div>
                <label htmlFor="primaryCardholderIndicator" className="block text-sm font-medium text-gray-700 mb-1">
                  Primary Cardholder <span className="text-red-500">*</span>
                </label>
                <select
                  id="primaryCardholderIndicator"
                  name="primaryCardholderIndicator"
                  value={formData.primaryCardholderIndicator}
                  onChange={handleInputChange}
                  className="flex h-10 w-full rounded-md border border-gray-300 bg-white px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:cursor-not-allowed disabled:opacity-50"
                  disabled={submitting}
                >
                  <option value="Y">Yes</option>
                  <option value="N">No</option>
                </select>
              </div>

              {/* First Name */}
              <div>
                <label htmlFor="firstName" className="block text-sm font-medium text-gray-700 mb-1">
                  First Name <span className="text-red-500">*</span>
                </label>
                <Input
                  id="firstName"
                  name="firstName"
                  type="text"
                  value={formData.firstName}
                  onChange={handleInputChange}
                  placeholder="Enter first name"
                  className={fieldErrors.firstName ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.firstName && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.firstName}</p>
                )}
              </div>

              {/* Middle Name */}
              <div>
                <label htmlFor="middleName" className="block text-sm font-medium text-gray-700 mb-1">
                  Middle Name
                </label>
                <Input
                  id="middleName"
                  name="middleName"
                  type="text"
                  value={formData.middleName}
                  onChange={handleInputChange}
                  placeholder="Enter middle name"
                  disabled={submitting}
                />
              </div>

              {/* Last Name */}
              <div>
                <label htmlFor="lastName" className="block text-sm font-medium text-gray-700 mb-1">
                  Last Name <span className="text-red-500">*</span>
                </label>
                <Input
                  id="lastName"
                  name="lastName"
                  type="text"
                  value={formData.lastName}
                  onChange={handleInputChange}
                  placeholder="Enter last name"
                  className={fieldErrors.lastName ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.lastName && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.lastName}</p>
                )}
              </div>

              {/* Date of Birth */}
              <div>
                <label htmlFor="dateOfBirth" className="block text-sm font-medium text-gray-700 mb-1">
                  Date of Birth <span className="text-red-500">*</span>
                </label>
                <Input
                  id="dateOfBirth"
                  name="dateOfBirth"
                  type="date"
                  value={formData.dateOfBirth}
                  onChange={handleInputChange}
                  className={fieldErrors.dateOfBirth ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.dateOfBirth && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.dateOfBirth}</p>
                )}
              </div>
            </div>
          </div>

          {/* Identification Section */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Identification
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* SSN */}
              <div>
                <label htmlFor="ssn" className="block text-sm font-medium text-gray-700 mb-1">
                  Social Security Number <span className="text-red-500">*</span>
                </label>
                <Input
                  id="ssn"
                  name="ssn"
                  type="text"
                  value={formData.ssn}
                  onChange={handleInputChange}
                  placeholder="123456789"
                  maxLength={11}
                  className={fieldErrors.ssn ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.ssn && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.ssn}</p>
                )}
              </div>

              {/* Government Issued ID */}
              <div>
                <label htmlFor="governmentIssuedId" className="block text-sm font-medium text-gray-700 mb-1">
                  Government Issued ID <span className="text-red-500">*</span>
                </label>
                <Input
                  id="governmentIssuedId"
                  name="governmentIssuedId"
                  type="text"
                  value={formData.governmentIssuedId}
                  onChange={handleInputChange}
                  placeholder="Enter ID number"
                  className={fieldErrors.governmentIssuedId ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.governmentIssuedId && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.governmentIssuedId}</p>
                )}
              </div>

              {/* FICO Credit Score */}
              <div>
                <label htmlFor="ficoCreditScore" className="block text-sm font-medium text-gray-700 mb-1">
                  FICO Credit Score <span className="text-red-500">*</span>
                </label>
                <Input
                  id="ficoCreditScore"
                  name="ficoCreditScore"
                  type="number"
                  min="300"
                  max="850"
                  value={formData.ficoCreditScore}
                  onChange={handleInputChange}
                  placeholder="300-850"
                  className={fieldErrors.ficoCreditScore ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.ficoCreditScore && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.ficoCreditScore}</p>
                )}
              </div>

              {/* EFT Account ID */}
              <div>
                <label htmlFor="eftAccountId" className="block text-sm font-medium text-gray-700 mb-1">
                  EFT Account ID <span className="text-red-500">*</span>
                </label>
                <Input
                  id="eftAccountId"
                  name="eftAccountId"
                  type="text"
                  value={formData.eftAccountId}
                  onChange={handleInputChange}
                  placeholder="Enter EFT account ID"
                  className={fieldErrors.eftAccountId ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.eftAccountId && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.eftAccountId}</p>
                )}
              </div>
            </div>
          </div>

          {/* Contact Information Section */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Contact Information
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* Phone Number 1 */}
              <div>
                <label htmlFor="phoneNumber1" className="block text-sm font-medium text-gray-700 mb-1">
                  Primary Phone Number <span className="text-red-500">*</span>
                </label>
                <Input
                  id="phoneNumber1"
                  name="phoneNumber1"
                  type="tel"
                  value={formData.phoneNumber1}
                  onChange={handleInputChange}
                  placeholder="(555) 123-4567"
                  className={fieldErrors.phoneNumber1 ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.phoneNumber1 && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.phoneNumber1}</p>
                )}
              </div>

              {/* Phone Number 2 */}
              <div>
                <label htmlFor="phoneNumber2" className="block text-sm font-medium text-gray-700 mb-1">
                  Secondary Phone Number
                </label>
                <Input
                  id="phoneNumber2"
                  name="phoneNumber2"
                  type="tel"
                  value={formData.phoneNumber2}
                  onChange={handleInputChange}
                  placeholder="(555) 123-4567"
                  disabled={submitting}
                />
              </div>
            </div>
          </div>

          {/* Address Section */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Address
            </h2>
            <div className="grid grid-cols-1 gap-6">
              {/* Address Line 1 */}
              <div>
                <label htmlFor="addressLine1" className="block text-sm font-medium text-gray-700 mb-1">
                  Address Line 1 <span className="text-red-500">*</span>
                </label>
                <Input
                  id="addressLine1"
                  name="addressLine1"
                  type="text"
                  value={formData.addressLine1}
                  onChange={handleInputChange}
                  placeholder="Street address"
                  className={fieldErrors.addressLine1 ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.addressLine1 && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.addressLine1}</p>
                )}
              </div>

              {/* Address Line 2 */}
              <div>
                <label htmlFor="addressLine2" className="block text-sm font-medium text-gray-700 mb-1">
                  Address Line 2
                </label>
                <Input
                  id="addressLine2"
                  name="addressLine2"
                  type="text"
                  value={formData.addressLine2}
                  onChange={handleInputChange}
                  placeholder="Apartment, suite, unit, etc."
                  disabled={submitting}
                />
              </div>

              {/* Address Line 3 */}
              <div>
                <label htmlFor="addressLine3" className="block text-sm font-medium text-gray-700 mb-1">
                  Address Line 3
                </label>
                <Input
                  id="addressLine3"
                  name="addressLine3"
                  type="text"
                  value={formData.addressLine3}
                  onChange={handleInputChange}
                  placeholder="Additional address information"
                  disabled={submitting}
                />
              </div>

              <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
                {/* State Code */}
                <div>
                  <label htmlFor="stateCode" className="block text-sm font-medium text-gray-700 mb-1">
                    State Code <span className="text-red-500">*</span>
                  </label>
                  <Input
                    id="stateCode"
                    name="stateCode"
                    type="text"
                    value={formData.stateCode}
                    onChange={handleInputChange}
                    placeholder="CA"
                    maxLength={2}
                    className={fieldErrors.stateCode ? 'border-red-500' : ''}
                    disabled={submitting}
                  />
                  {fieldErrors.stateCode && (
                    <p className="mt-1 text-sm text-red-600">{fieldErrors.stateCode}</p>
                  )}
                </div>

                {/* Zip Code */}
                <div>
                  <label htmlFor="zipCode" className="block text-sm font-medium text-gray-700 mb-1">
                    Zip Code <span className="text-red-500">*</span>
                  </label>
                  <Input
                    id="zipCode"
                    name="zipCode"
                    type="text"
                    value={formData.zipCode}
                    onChange={handleInputChange}
                    placeholder="12345"
                    className={fieldErrors.zipCode ? 'border-red-500' : ''}
                    disabled={submitting}
                  />
                  {fieldErrors.zipCode && (
                    <p className="mt-1 text-sm text-red-600">{fieldErrors.zipCode}</p>
                  )}
                </div>

                {/* Country Code */}
                <div>
                  <label htmlFor="countryCode" className="block text-sm font-medium text-gray-700 mb-1">
                    Country Code <span className="text-red-500">*</span>
                  </label>
                  <Input
                    id="countryCode"
                    name="countryCode"
                    type="text"
                    value={formData.countryCode}
                    onChange={handleInputChange}
                    placeholder="US"
                    maxLength={2}
                    className={fieldErrors.countryCode ? 'border-red-500' : ''}
                    disabled={submitting}
                  />
                  {fieldErrors.countryCode && (
                    <p className="mt-1 text-sm text-red-600">{fieldErrors.countryCode}</p>
                  )}
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
              {submitting ? 'Creating...' : 'Create Customer'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default NewCustomerPage;
